package org.cardanofoundation.explorer.api.service.impl;

import java.math.BigInteger;
import java.sql.Timestamp;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

import org.cardanofoundation.explorer.api.common.constant.CommonConstant;
import org.cardanofoundation.explorer.api.common.enumeration.AnalyticType;
import org.cardanofoundation.explorer.api.exception.BusinessCode;
import org.cardanofoundation.explorer.api.exception.NoContentException;
import org.cardanofoundation.explorer.api.mapper.AddressMapper;
import org.cardanofoundation.explorer.api.mapper.AssetMetadataMapper;
import org.cardanofoundation.explorer.api.mapper.TokenMapper;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.address.AddressChartBalanceData;
import org.cardanofoundation.explorer.api.model.response.address.AddressChartBalanceResponse;
import org.cardanofoundation.explorer.api.model.response.address.AddressFilterResponse;
import org.cardanofoundation.explorer.api.model.response.address.AddressResponse;
import org.cardanofoundation.explorer.api.model.response.contract.ContractFilterResponse;
import org.cardanofoundation.explorer.api.model.response.token.TokenAddressResponse;
import org.cardanofoundation.explorer.api.repository.ledgersync.AddressRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.AddressTokenBalanceRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.AddressTxBalanceRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.AggregateAddressTxBalanceRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.AssetMetadataRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.ScriptRepository;
import org.cardanofoundation.explorer.api.service.AddressService;
import org.cardanofoundation.explorer.api.util.AddressUtils;
import org.cardanofoundation.explorer.api.util.DataUtil;
import org.cardanofoundation.explorer.api.util.DateUtils;
import org.cardanofoundation.explorer.common.entity.enumeration.ScriptType;
import org.cardanofoundation.explorer.common.entity.ledgersync.Address;
import org.cardanofoundation.explorer.common.entity.ledgersync.aggregation.AggregateAddressTxBalance;
import org.cardanofoundation.explorer.common.exception.BusinessException;

@Service
@RequiredArgsConstructor
@Log4j2
public class AddressServiceImpl implements AddressService {

  private final AddressTxBalanceRepository addressTxBalanceRepository;
  private final AddressRepository addressRepository;
  private final AssetMetadataRepository assetMetadataRepository;
  private final AddressTokenBalanceRepository addressTokenBalanceRepository;
  private final TokenMapper tokenMapper;
  private final AddressMapper addressMapper;
  private final AssetMetadataMapper assetMetadataMapper;
  private final ScriptRepository scriptRepository;
  private final AggregateAddressTxBalanceRepository aggregateAddressTxBalanceRepository;

  @Value("${application.network}")
  private String network;

  @Override
  public AddressResponse getAddressDetail(String address) {
    Address addr =
        addressRepository
            .findFirstByAddress(address)
            .orElse(
                Address.builder().address(address).txCount(0L).balance(BigInteger.ZERO).build());
    final int ADDRESS_MIN_LENGTH = 56;
    if (!checkNetworkAddress(address) || address.length() < ADDRESS_MIN_LENGTH) {
      throw new BusinessException(BusinessCode.ADDRESS_NOT_FOUND);
    }
    AddressResponse addressResponse = addressMapper.fromAddress(addr);
    addressResponse.setStakeAddress(AddressUtils.checkStakeAddress(address));
    addressResponse.setScriptHash(AddressUtils.getHexPaymentPart(address));
    setAssociatedScript(addressResponse);
    return addressResponse;
  }

  /**
   * Set associated script for address (native script or smart contract)
   *
   * @param addressResponse
   */
  private void setAssociatedScript(AddressResponse addressResponse) {
    if (addressResponse.getScriptHash() != null) {
      scriptRepository
          .findByHash(addressResponse.getScriptHash())
          .ifPresent(
              script -> {
                if (ScriptType.PLUTUSV1.equals(script.getType())
                    || ScriptType.PLUTUSV2.equals(script.getType())
                    || ScriptType.PLUTUSV3.equals(script.getType())) {
                  addressResponse.setAssociatedSmartContract(Boolean.TRUE);
                } else {
                  addressResponse.setAssociatedNativeScript(Boolean.TRUE);
                }
              });
    }
  }

  /**
   * Check address is valid in this network
   *
   * @param address address view value
   * @return true if valid and false if not
   */
  private boolean checkNetworkAddress(String address) {
    if (address.startsWith(CommonConstant.TESTNET_ADDRESS_PREFIX)) {
      return !network.equals(CommonConstant.MAINNET_NETWORK);
    } else {
      return network.equals(CommonConstant.MAINNET_NETWORK);
    }
  }

  @Override
  public AddressChartBalanceResponse getAddressAnalytics(String address, AnalyticType type) {
    Address addr =
        addressRepository
            .findFirstByAddress(address)
            .orElseThrow(() -> new NoContentException(BusinessCode.ADDRESS_NOT_FOUND));
    AddressChartBalanceResponse response = new AddressChartBalanceResponse();

    if (Long.valueOf(0).equals(addr.getTxCount())) {
      return AddressChartBalanceResponse.builder()
          .highestBalance(BigInteger.ZERO)
          .lowestBalance(BigInteger.ZERO)
          .data(Collections.emptyList())
          .build();
    }
    List<LocalDateTime> dates = DateUtils.getListDateAnalytic(type);

    List<AddressChartBalanceData> data = new ArrayList<>();
    if (AnalyticType.ONE_DAY.equals(type)) {
      var fromBalance =
          aggregateAddressTxBalanceRepository
              .sumBalanceByAddressId(addr.getId(), dates.get(0).minusDays(1).toLocalDate())
              .orElse(BigInteger.ZERO);
      getHighestAndLowestBalance(addr, fromBalance, dates, response);

      data.add(new AddressChartBalanceData(dates.get(0), fromBalance));
      for (int i = 1; i < dates.size(); i++) {
        Optional<BigInteger> balance =
            addressTxBalanceRepository.getBalanceByAddressAndTime(
                addr, Timestamp.valueOf(dates.get(i - 1)), Timestamp.valueOf(dates.get(i)));
        if (balance.isPresent()) {
          fromBalance = fromBalance.add(balance.get());
        }
        data.add(new AddressChartBalanceData(dates.get(i), fromBalance));
      }
      response.setData(data);
    } else {
      // Remove last date because we will get data of today
      dates.remove(0);

      var fromBalance =
          aggregateAddressTxBalanceRepository
              .sumBalanceByAddressId(addr.getId(), dates.get(0).minusDays(1).toLocalDate())
              .orElse(BigInteger.ZERO);
      getHighestAndLowestBalance(addr, fromBalance, dates, response);

      List<AggregateAddressTxBalance> aggregateAddressTxBalances =
          aggregateAddressTxBalanceRepository.findAllByAddressIdAndDayBetween(
              addr.getId(), dates.get(0).toLocalDate(), dates.get(dates.size() - 1).toLocalDate());

      // Data in aggregate_address_tx_balance save at end of day, but we will display start of day
      // So we need to add 1 day to display correct data
      Map<LocalDate, BigInteger> mapBalance =
          aggregateAddressTxBalances.stream()
              .collect(
                  Collectors.toMap(
                      balance -> balance.getDay().plusDays(1),
                      AggregateAddressTxBalance::getBalance));
      for (LocalDateTime date : dates) {
        if (mapBalance.containsKey(date.toLocalDate())) {
          fromBalance = fromBalance.add(mapBalance.get(date.toLocalDate()));
        }
        data.add(new AddressChartBalanceData(date, fromBalance));
      }
      response.setData(data);
    }
    return response;
  }

  /**
   * Get highest and lowest balance of address
   *
   * @param addr address
   * @param fromBalance balance of address at start date
   * @param dates list date
   * @param response chart response
   */
  private void getHighestAndLowestBalance(
      Address addr,
      BigInteger fromBalance,
      List<LocalDateTime> dates,
      AddressChartBalanceResponse response) {
    var minMaxBalance =
        addressTxBalanceRepository.findMinMaxBalanceByAddress(
            addr.getId(),
            fromBalance,
            Timestamp.valueOf(dates.get(0)),
            Timestamp.valueOf(dates.get(dates.size() - 1)));
    if (minMaxBalance.getMaxVal().compareTo(fromBalance) > 0) {
      response.setHighestBalance(minMaxBalance.getMaxVal());
    } else {
      response.setHighestBalance(fromBalance);
    }
    if (minMaxBalance.getMinVal().compareTo(fromBalance) < 0) {
      response.setLowestBalance(minMaxBalance.getMinVal());
    } else {
      response.setLowestBalance(fromBalance);
    }
  }

  @Override
  public BaseFilterResponse<ContractFilterResponse> getContracts(Pageable pageable) {
    Page<Address> contractPage = addressRepository.findAllByAddressHasScriptIsTrue(pageable);
    Page<ContractFilterResponse> pageResponse =
        contractPage.map(addressMapper::fromAddressToContractFilter);
    return new BaseFilterResponse<>(pageResponse);
  }

  @Override
  public BaseFilterResponse<AddressFilterResponse> getTopAddress(Pageable pageable) {
    List<Address> addressPage = addressRepository.findAllOrderByBalance(pageable);
    List<AddressFilterResponse> responses =
        addressPage.stream()
            .map(addressMapper::fromAddressToFilterResponse)
            .collect(Collectors.toList());
    Page<AddressFilterResponse> pageResponse =
        new PageImpl<>(responses, pageable, pageable.getPageSize());
    return new BaseFilterResponse<>(pageResponse);
  }

  /**
   * Get list token by display name
   *
   * @param pageable page information
   * @param address wallet address
   * @param displayName display name of token
   * @return list token by display name
   */
  @Override
  public BaseFilterResponse<TokenAddressResponse> getTokenByDisplayName(
      Pageable pageable, String address, String displayName) {
    Page<TokenAddressResponse> tokenListResponse;
    Address addr =
        addressRepository
            .findFirstByAddress(address)
            .orElseThrow(() -> new NoContentException(BusinessCode.ADDRESS_NOT_FOUND));
    if (DataUtil.isNullOrEmpty(displayName)) {
      tokenListResponse =
          addressTokenBalanceRepository
              .findTokenAndBalanceByAddress(addr, pageable)
              .map(tokenMapper::fromAddressTokenProjection);
    } else {
      displayName = displayName.trim().toLowerCase();
      tokenListResponse =
          addressTokenBalanceRepository
              .findTokenAndBalanceByAddressAndNameView(addr, displayName, pageable)
              .map(tokenMapper::fromAddressTokenProjection);
    }
    return new BaseFilterResponse<>(tokenListResponse);
  }
}
