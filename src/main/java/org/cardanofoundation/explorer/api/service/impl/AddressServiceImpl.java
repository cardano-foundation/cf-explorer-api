package org.cardanofoundation.explorer.api.service.impl;

import java.math.BigInteger;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

import org.cardanofoundation.conversions.CardanoConverters;
import org.cardanofoundation.explorer.api.common.constant.CommonConstant;
import org.cardanofoundation.explorer.api.common.constant.CommonConstant.NetworkType;
import org.cardanofoundation.explorer.api.common.enumeration.AnalyticType;
import org.cardanofoundation.explorer.api.exception.BusinessCode;
import org.cardanofoundation.explorer.api.mapper.TokenMapper;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.address.AddressChartBalanceData;
import org.cardanofoundation.explorer.api.model.response.address.AddressChartBalanceResponse;
import org.cardanofoundation.explorer.api.model.response.address.AddressResponse;
import org.cardanofoundation.explorer.api.model.response.token.TokenAddressResponse;
import org.cardanofoundation.explorer.api.projection.AddressTokenProjection;
import org.cardanofoundation.explorer.api.repository.ledgersync.MultiAssetRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.ScriptRepository;
import org.cardanofoundation.explorer.api.repository.ledgersyncagg.AddressRepository;
import org.cardanofoundation.explorer.api.repository.ledgersyncagg.AddressTxAmountRepository;
import org.cardanofoundation.explorer.api.repository.ledgersyncagg.AddressTxCountRepository;
import org.cardanofoundation.explorer.api.repository.ledgersyncagg.AggregateAddressTxBalanceRepository;
import org.cardanofoundation.explorer.api.repository.ledgersyncagg.LatestTokenBalanceRepository;
import org.cardanofoundation.explorer.api.service.AddressService;
import org.cardanofoundation.explorer.api.util.AddressUtils;
import org.cardanofoundation.explorer.api.util.DataUtil;
import org.cardanofoundation.explorer.api.util.DateUtils;
import org.cardanofoundation.explorer.common.entity.enumeration.ScriptType;
import org.cardanofoundation.explorer.common.entity.ledgersyncsagg.Address;
import org.cardanofoundation.explorer.common.entity.ledgersyncsagg.AddressTxCount;
import org.cardanofoundation.explorer.common.entity.ledgersyncsagg.AggregateAddressTxBalance;
import org.cardanofoundation.explorer.common.entity.ledgersyncsagg.LatestTokenBalance;
import org.cardanofoundation.explorer.common.exception.BusinessException;

@Service
@RequiredArgsConstructor
@Log4j2
public class AddressServiceImpl implements AddressService {

  private final AddressRepository addressRepository;
  private final LatestTokenBalanceRepository latestTokenBalanceRepository;
  private final TokenMapper tokenMapper;
  private final ScriptRepository scriptRepository;
  private final AggregateAddressTxBalanceRepository aggregateAddressTxBalanceRepository;
  private final AddressTxCountRepository addressTxCountRepository;
  private final AddressTxAmountRepository addressTxAmountRepository;
  private final MultiAssetRepository multiAssetRepository;
  private final CardanoConverters cardanoConverters;

  @Value("${application.network}")
  private String network;

  @Override
  public AddressResponse getAddressDetail(String address) {
    final int ADDRESS_MIN_LENGTH = 56;
    if (!checkNetworkAddress(address) || address.length() < ADDRESS_MIN_LENGTH) {
      throw new BusinessException(BusinessCode.ADDRESS_NOT_FOUND);
    }

    AddressResponse addressResponse =
        AddressResponse.fromProjection(addressRepository.getAddressDetail(address));

    if (addressResponse == null) {
      addressResponse =
          AddressResponse.builder().address(address).txCount(0L).balance(BigInteger.ZERO).build();
    }

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
      return !network.equals(NetworkType.MAINNET);
    } else if (address.startsWith(CommonConstant.ADDRESS_PREFIX)) {
      return network.equals(NetworkType.MAINNET);
    } else { // Genesis address
      return true;
    }
  }

  @Override
  public AddressChartBalanceResponse getAddressAnalytics(String address, AnalyticType type) {
    Address addr =
        addressRepository
            .findFirstByAddress(address)
            .orElseThrow(() -> new BusinessException(BusinessCode.ADDRESS_NOT_FOUND));
    AddressChartBalanceResponse response = new AddressChartBalanceResponse();

    AddressTxCount addressTxCount =
        addressTxCountRepository
            .findById(address)
            .orElse(AddressTxCount.builder().address(address).txCount(0L).build());

    if (Long.valueOf(0).equals(addressTxCount.getTxCount())) {
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
          addressTxAmountRepository
              .sumBalanceByAddress(addr.getAddress(), cardanoConverters.time().toSlot(dates.get(0)))
              .orElse(BigInteger.ZERO);
      getHighestAndLowestBalance(addr, fromBalance, dates, response);

      data.add(new AddressChartBalanceData(dates.get(0), fromBalance));
      for (int i = 1; i < dates.size(); i++) {
        long fromSlot = cardanoConverters.time().toSlot(dates.get(i - 1));
        long toSlot = cardanoConverters.time().toSlot(dates.get(i));

        Optional<BigInteger> balance =
            addressTxAmountRepository.getBalanceByAddressAndSlotBetween(
                addr.getAddress(), fromSlot, toSlot);

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
          addressTxAmountRepository
              .sumBalanceByAddress(addr.getAddress(), cardanoConverters.time().toSlot(dates.get(0)))
              .orElse(BigInteger.ZERO);
      getHighestAndLowestBalance(addr, fromBalance, dates, response);

      List<AggregateAddressTxBalance> aggregateAddressTxBalances =
          aggregateAddressTxBalanceRepository.findAllByAddressIdAndDayBetween(
              addr.getAddress(),
              dates.get(0).toLocalDate(),
              dates.get(dates.size() - 1).toLocalDate());

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
    long fromSlot = cardanoConverters.time().toSlot(dates.get(0));
    long toSlot = cardanoConverters.time().toSlot(dates.get(dates.size() - 1));

    var minMaxBalance =
        addressTxAmountRepository.findMinMaxBalanceByAddress(
            addr.getAddress(), fromBalance, fromSlot, toSlot);

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

    List<TokenAddressResponse> tokenListResponse = new ArrayList<>();
    List<LatestTokenBalance> latestTokenBalances =
        latestTokenBalanceRepository.findAllByAddress(address);

    Set<String> unitSet =
        latestTokenBalances.stream().map(LatestTokenBalance::getUnit).collect(Collectors.toSet());

    Map<String, AddressTokenProjection> tokenMetadataMap =
        multiAssetRepository.findTokenMetadataByUnitIn(unitSet).stream()
            .collect(Collectors.toMap(AddressTokenProjection::getUnit, Function.identity()));

    boolean isSearchByDisplayName;
    if (!DataUtil.isNullOrEmpty(displayName)) {
      isSearchByDisplayName = true;
      displayName = displayName.trim().toLowerCase();
    } else {
      isSearchByDisplayName = false;
    }

    for (LatestTokenBalance latestTokenBalance : latestTokenBalances) {
      AddressTokenProjection projection = tokenMetadataMap.get(latestTokenBalance.getUnit());
      TokenAddressResponse tokenAddressResponse =
          tokenMapper.fromAddressTokenProjection(projection);
      tokenAddressResponse.setQuantity(latestTokenBalance.getQuantity());
      tokenAddressResponse.setAddress(address);

      if (!isSearchByDisplayName) {
        tokenListResponse.add(tokenAddressResponse);
      } else if (tokenAddressResponse.getDisplayName().toLowerCase().contains(displayName)
          || tokenAddressResponse.getFingerprint().equals(displayName)) {
        tokenListResponse.add(tokenAddressResponse);
      }
    }

    return new BaseFilterResponse<>(BaseFilterResponse.getPageImpl(tokenListResponse, pageable));
  }
}
