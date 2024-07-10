package org.cardanofoundation.explorer.api.service.impl;

import java.math.BigInteger;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
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
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

import org.cardanofoundation.explorer.api.common.constant.CommonConstant;
import org.cardanofoundation.explorer.api.common.enumeration.AnalyticType;
import org.cardanofoundation.explorer.api.exception.BusinessCode;
import org.cardanofoundation.explorer.api.mapper.TokenMapper;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.address.AddressChartBalanceData;
import org.cardanofoundation.explorer.api.model.response.address.AddressChartBalanceResponse;
import org.cardanofoundation.explorer.api.model.response.address.AddressFilterResponse;
import org.cardanofoundation.explorer.api.model.response.address.AddressResponse;
import org.cardanofoundation.explorer.api.model.response.token.TokenAddressResponse;
import org.cardanofoundation.explorer.api.projection.AddressQuantityDayProjection;
import org.cardanofoundation.explorer.api.repository.ledgersync.AddressRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.AddressTxAmountRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.MultiAssetRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.ScriptRepository;
import org.cardanofoundation.explorer.api.service.AddressService;
import org.cardanofoundation.explorer.api.util.AddressUtils;
import org.cardanofoundation.explorer.api.util.DataUtil;
import org.cardanofoundation.explorer.api.util.DateUtils;
import org.cardanofoundation.explorer.common.entity.enumeration.ScriptType;
import org.cardanofoundation.explorer.common.entity.ledgersync.Address;
import org.cardanofoundation.explorer.common.exception.BusinessException;

@Service
@RequiredArgsConstructor
@Log4j2
public class AddressServiceImpl implements AddressService {

  private final AddressRepository addressRepository;
  private final MultiAssetRepository multiAssetRepository;
  private final TokenMapper tokenMapper;
  private final ScriptRepository scriptRepository;
  private final AddressTxAmountRepository addressTxAmountRepository;

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
            .orElseThrow(() -> new BusinessException(BusinessCode.ADDRESS_NOT_FOUND));
    AddressChartBalanceResponse response = new AddressChartBalanceResponse();

    Long txCount = addressTxAmountRepository.getTxCountForAddress(address).orElse(0L);
    if (Long.valueOf(0).equals(txCount)) {
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
              .sumBalanceByAddress(addr.getAddress(), dates.get(0).toEpochSecond(ZoneOffset.UTC))
              .orElse(BigInteger.ZERO);
      getHighestAndLowestBalance(addr, fromBalance, dates, response);

      data.add(new AddressChartBalanceData(dates.get(0), fromBalance));
      for (int i = 1; i < dates.size(); i++) {
        Optional<BigInteger> balance =
            addressTxAmountRepository.getBalanceByAddressAndTime(
                addr.getAddress(),
                dates.get(i - 1).toInstant(ZoneOffset.UTC).getEpochSecond(),
                dates.get(i).toInstant(ZoneOffset.UTC).getEpochSecond());

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
              .sumBalanceByAddress(addr.getAddress(), dates.get(0).toEpochSecond(ZoneOffset.UTC))
              .orElse(BigInteger.ZERO);
      getHighestAndLowestBalance(addr, fromBalance, dates, response);
      long from = dates.get(0).toInstant(ZoneOffset.UTC).getEpochSecond();
      long to = dates.get(dates.size() - 1).toInstant(ZoneOffset.UTC).getEpochSecond();
      List<AddressQuantityDayProjection> allByAddressAndDayBetween =
          addressTxAmountRepository.findAllByAddressAndDayBetween(addr.getAddress(), from, to);

      // Data in aggregate_address_tx_balance save at end of day, but we will display start of day
      // So we need to add 1 day to display correct data
      Map<LocalDate, BigInteger> mapBalance =
          allByAddressAndDayBetween.stream()
              .collect(
                  Collectors.toMap(
                      aggBalance ->
                          aggBalance.getDay().atZone(ZoneOffset.UTC).toLocalDate().plusDays(1),
                      AddressQuantityDayProjection::getQuantity));
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
        addressTxAmountRepository.findMinMaxBalanceByAddress(
            addr.getAddress(),
            fromBalance,
            dates.get(0).toInstant(ZoneOffset.UTC).getEpochSecond(),
            dates.get(dates.size() - 1).toInstant(ZoneOffset.UTC).getEpochSecond());
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

  //  @Override
  //  public BaseFilterResponse<ContractFilterResponse> getContracts(Pageable pageable) {
  //    Page<Address> contractPage = addressRepository.findAllByAddressHasScriptIsTrue(pageable);
  //    Page<ContractFilterResponse> pageResponse =
  //        contractPage.map(addressMapper::fromAddressToContractFilter);
  //    return new BaseFilterResponse<>(pageResponse);
  //  }
  @Override
  public BaseFilterResponse<AddressFilterResponse> getTopAddress(Pageable pageable) {
    // TODO will be removed for release 1.3, need to be fixed in next release

    //    List<LatestAddressBalance> latestAddressBalances =
    //        latestAddressBalanceRepository.findAllLatestAddressBalance(pageable);
    //    Page<LatestAddressBalance> latestAddressBalancePage =
    //        new PageImpl<>(latestAddressBalances, pageable, pageable.getPageSize());
    //    List<AddressTxCountProjection> addressTxCountProjections =
    // addressTxAmountRepository.getTxCountListForAddresses(latestAddressBalancePage.stream().map(LatestAddressBalance::getAddress).toList());
    //    Map<String, Long> addressTxCountMap = addressTxCountProjections
    //            .stream().collect(Collectors.toMap(AddressTxCountProjection::getAddress,
    // AddressTxCountProjection::getTxCount));
    //    List<AddressFilterResponse> addressFilterResponses =
    //        latestAddressBalancePage.stream()
    //            .map(
    //                latestAddressBalance ->
    //                    AddressFilterResponse.builder()
    //                        .address(latestAddressBalance.getAddress())
    //                        .balance(latestAddressBalance.getQuantity())
    //                        .txCount(
    //                            addressTxCountMap.getOrDefault(latestAddressBalance.getAddress(),
    // 0L))
    //                        .build())
    //            .collect(Collectors.toList());
    //    return new BaseFilterResponse<>(latestAddressBalancePage, addressFilterResponses);
    return new BaseFilterResponse<>();
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

    if (DataUtil.isNullOrEmpty(displayName)) {
      tokenListResponse =
          multiAssetRepository
              .findTokenAndBalanceByAddress(address, pageable)
              .map(tokenMapper::fromAddressTokenProjection);
    } else {
      displayName = displayName.trim().toLowerCase();
      tokenListResponse =
          multiAssetRepository
              .findTokenAndBalanceByAddressAndNameView(address, displayName, pageable)
              .map(tokenMapper::fromAddressTokenProjection);
    }
    return new BaseFilterResponse<>(tokenListResponse);
  }
}
