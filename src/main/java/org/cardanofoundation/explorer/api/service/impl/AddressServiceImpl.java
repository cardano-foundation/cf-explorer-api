package org.cardanofoundation.explorer.api.service.impl;

import com.bloxbean.cardano.client.transaction.spec.script.NativeScript;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.Collections;
import org.apache.commons.codec.binary.Hex;
import org.cardanofoundation.explorer.api.common.constant.CommonConstant;
import org.cardanofoundation.explorer.api.common.enumeration.AnalyticType;
import org.cardanofoundation.explorer.api.exception.BusinessCode;
import org.cardanofoundation.explorer.api.mapper.AddressMapper;
import org.cardanofoundation.explorer.api.mapper.AssetMetadataMapper;
import org.cardanofoundation.explorer.api.mapper.TokenMapper;
import org.cardanofoundation.explorer.api.model.request.ScriptVerifyRequest;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.address.AddressAnalyticsResponse;
import org.cardanofoundation.explorer.api.model.response.address.AddressFilterResponse;
import org.cardanofoundation.explorer.api.model.response.address.AddressResponse;
import org.cardanofoundation.explorer.api.model.response.contract.ContractFilterResponse;
import org.cardanofoundation.explorer.api.model.response.token.TokenAddressResponse;
import org.cardanofoundation.explorer.api.projection.AddressTokenProjection;
import org.cardanofoundation.explorer.api.projection.MinMaxProjection;
import org.cardanofoundation.explorer.api.repository.AddressTokenBalanceRepository;
import org.cardanofoundation.explorer.api.repository.AddressRepository;
import org.cardanofoundation.explorer.api.repository.AddressTxBalanceRepository;
import org.cardanofoundation.explorer.api.repository.AggregateAddressTxBalanceRepository;
import org.cardanofoundation.explorer.api.repository.AssetMetadataRepository;
import org.cardanofoundation.explorer.api.repository.MultiAssetRepository;
import org.cardanofoundation.explorer.api.repository.ScriptRepository;
import org.cardanofoundation.explorer.api.service.AddressService;
import org.cardanofoundation.explorer.api.util.AddressUtils;
import org.cardanofoundation.explorer.api.util.HexUtils;
import org.cardanofoundation.explorer.common.exceptions.NoContentException;
import org.cardanofoundation.explorer.consumercommon.entity.Address;
import org.cardanofoundation.explorer.consumercommon.entity.AssetMetadata;
import org.cardanofoundation.explorer.consumercommon.entity.MultiAsset;
import org.cardanofoundation.explorer.common.exceptions.BusinessException;
import java.math.BigInteger;
import java.sql.Timestamp;
import java.time.Instant;
import java.time.LocalDate;
import java.time.LocalTime;
import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.apache.commons.lang3.StringUtils;
import org.cardanofoundation.explorer.consumercommon.entity.Script;
import org.cardanofoundation.ledgersync.common.common.address.ShelleyAddress;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@RequiredArgsConstructor
@Log4j2
public class AddressServiceImpl implements AddressService {

  private final MultiAssetRepository multiAssetRepository;
  private final AddressTxBalanceRepository addressTxBalanceRepository;
  private final AddressRepository addressRepository;
  private final AssetMetadataRepository assetMetadataRepository;
  private final AddressTokenBalanceRepository addressTokenBalanceRepository;
  private final TokenMapper tokenMapper;
  private final AddressMapper addressMapper;
  private final AssetMetadataMapper assetMetadataMapper;
  private final ScriptRepository scriptRepository;
  private final AggregateAddressTxBalanceRepository aggregateAddressTxBalanceRepository;

  static final Integer ADDRESS_ANALYTIC_BALANCE_NUMBER = 5;
  static final String SCRIPT_NOT_VERIFIED = "Script not verified";


  @Value("${application.network}")
  private String network;


  @Override
  @Transactional(readOnly = true)
  public AddressResponse getAddressDetail(String address) {
    Address addr = addressRepository.findFirstByAddress(address).orElse(
        Address.builder().address(address).txCount(0L).balance(BigInteger.ZERO).build()
    );
    if(!checkNetworkAddress(address)) {
      throw new NoContentException(BusinessCode.ADDRESS_NOT_FOUND);
    }
    AddressResponse addressResponse = addressMapper.fromAddress(addr);
    addressResponse.setStakeAddress(AddressUtils.checkStakeAddress(address));
    return addressResponse;
  }

  /**
   * Check address is valid in this network
   *
   * @param address address view value
   * @return true if valid and false if not
   */
  private boolean checkNetworkAddress(String address) {
    if (network.equals(CommonConstant.MAINNET_NETWORK)) {
      return !address.startsWith(CommonConstant.TESTNET_ADDRESS_PREFIX);
    } else {
      return address.startsWith(CommonConstant.TESTNET_ADDRESS_PREFIX);
    }
  }

  @Override
  @Transactional(readOnly = true)
  public List<AddressAnalyticsResponse> getAddressAnalytics(String address, AnalyticType type)
      throws ExecutionException, InterruptedException {
    Address addr = addressRepository.findFirstByAddress(address)
        .orElseThrow(() -> new NoContentException(BusinessCode.ADDRESS_NOT_FOUND));
    Long txCount = addressTxBalanceRepository.countByAddress(addr);
    if (Long.valueOf(0).equals(txCount)) {
      return List.of();
    }
    List<LocalDate> dates = getAnalyticDates(type);
    ExecutorService executorService = Executors.newFixedThreadPool(ADDRESS_ANALYTIC_BALANCE_NUMBER);

    final Optional<LocalDate> maxDateAgg = aggregateAddressTxBalanceRepository.getMaxDay();
    List<CompletableFuture<AddressAnalyticsResponse>> futureAddressAnalytics = new ArrayList<>();
    dates.forEach(dateItem -> futureAddressAnalytics.add(
        CompletableFuture.supplyAsync(() ->
            getBalanceOfAddress(addr, dateItem, maxDateAgg), executorService))
    );

    CompletableFuture.allOf(futureAddressAnalytics.toArray(new CompletableFuture[0])).join();
    List<AddressAnalyticsResponse> responses = new ArrayList<>();

    for (CompletableFuture<AddressAnalyticsResponse> addressAnalytic : futureAddressAnalytics) {
      responses.add(addressAnalytic.get());
    }

    return responses;
  }

  private List<LocalDate> getAnalyticDates(AnalyticType type) {
    LocalDate currentDate = LocalDate.ofInstant(Instant.now(), ZoneOffset.UTC);
    List<LocalDate> dates = new ArrayList<>();
    switch (type) {
      case ONE_WEEK:
        var currentWeek = LocalDate.ofInstant(Instant.now(), ZoneOffset.UTC);
        for (int i = ADDRESS_ANALYTIC_BALANCE_NUMBER - 1; i >= 0; i--) {
          dates.add(currentWeek.minusWeeks(i));
        }
        break;
      case ONE_MONTH:
        var currentMonth = LocalDate.ofInstant(Instant.now(), ZoneOffset.UTC);
        for (int i = ADDRESS_ANALYTIC_BALANCE_NUMBER - 1; i >= 0; i--) {
          dates.add(currentMonth.minusMonths(i));
        }
        break;
      case THREE_MONTH:
        for (int i = ADDRESS_ANALYTIC_BALANCE_NUMBER - 1; i >= 0; i--) {
          dates.add(currentDate.minusMonths(i * 3L));
        }
        break;
      default:
        for (int i = ADDRESS_ANALYTIC_BALANCE_NUMBER - 1; i >= 0; i--) {
          dates.add(currentDate.minusDays(i));
        }
    }
    return dates;
  }

  private AddressAnalyticsResponse getBalanceOfAddress(
      Address address,LocalDate to, Optional<LocalDate> maxDateAgg) {
    if (maxDateAgg.isEmpty()) {
      BigInteger balance = addressTxBalanceRepository.getBalanceByAddressAndTime(
          address,
          Timestamp.valueOf(to.atTime(LocalTime.MAX))
      ).orElse(BigInteger.ZERO);
      return new AddressAnalyticsResponse(to, balance);
    }

    BigInteger balance;
    if (LocalDate.now().isEqual(to)) {
      balance = getBalanceInRangeHaveToday(address, to, maxDateAgg.get());
    } else {
      balance = getBalanceInRangePreviousToday(address, to, maxDateAgg.get());
    }

    if (BigInteger.ZERO.equals(balance)) {
      Long numberBalanceRecord = addressTxBalanceRepository.countRecord(
          address, Timestamp.valueOf(to.atTime(LocalTime.MAX))
      );
      boolean isNoRecord = numberBalanceRecord == null || numberBalanceRecord ==  0;
      balance = isNoRecord ? null : balance;
    }

    return new AddressAnalyticsResponse(to, balance);
  }

  private BigInteger getBalanceInRangeHaveToday(
      Address address, LocalDate to, LocalDate maxDateAgg) {
    BigInteger todayBalance = addressTxBalanceRepository.getBalanceByAddressAndTime(
            address,
            Timestamp.valueOf(to.minusDays(1).atTime(LocalTime.MAX)),
            Timestamp.valueOf(to.atTime(LocalTime.MAX))
        )
        .orElse(BigInteger.ZERO);

    boolean isNotMissingAggregationData = !to.minusDays(1).isAfter(maxDateAgg);
    if (isNotMissingAggregationData) {
      BigInteger rangeToYesterdayBalance = aggregateAddressTxBalanceRepository
          .sumBalanceByAddressId(address.getId(), to.minusDays(1))
          .orElse(BigInteger.ZERO);
      return todayBalance.add(rangeToYesterdayBalance);
    } else {
      BigInteger balanceAgg = aggregateAddressTxBalanceRepository
          .sumBalanceByAddressId(address.getId(), maxDateAgg)
          .orElse(BigInteger.ZERO);

      BigInteger balanceNotAgg = addressTxBalanceRepository.getBalanceByAddressAndTime(
          address,
          Timestamp.valueOf(maxDateAgg.atTime(LocalTime.MAX)),
          Timestamp.valueOf(to.minusDays(1).atTime(LocalTime.MAX))
      ).orElse(BigInteger.ZERO);
      return todayBalance.add(balanceAgg).add(balanceNotAgg);
    }
  }

  private BigInteger getBalanceInRangePreviousToday(
      Address address, LocalDate to, LocalDate maxDateAgg) {
    boolean isNotMissingAggregationData = !to.isAfter(maxDateAgg);
    if (isNotMissingAggregationData) {
      return aggregateAddressTxBalanceRepository
          .sumBalanceByAddressId(address.getId(), to)
          .orElse(BigInteger.ZERO);
    }

    BigInteger balanceAgg = aggregateAddressTxBalanceRepository
        .sumBalanceByAddressId(address.getId(), maxDateAgg)
        .orElse(BigInteger.ZERO);

    BigInteger balanceNotAgg = addressTxBalanceRepository.getBalanceByAddressAndTime(
        address,
        Timestamp.valueOf(maxDateAgg.atTime(LocalTime.MAX)),
        Timestamp.valueOf(to.atTime(LocalTime.MAX))
    ).orElse(BigInteger.ZERO);
    return balanceAgg.add(balanceNotAgg);
  }

  @Override
  @Transactional(readOnly = true)
  public List<BigInteger> getAddressMinMaxBalance(String address) {
    Address addr = addressRepository.findFirstByAddress(address)
        .orElseThrow(() -> new NoContentException(BusinessCode.ADDRESS_NOT_FOUND));

    MinMaxProjection balanceList = addressTxBalanceRepository.findMinMaxBalanceByAddress(
        addr.getId());
    if (balanceList == null) {
      return Collections.emptyList();
    }
    return List.of(balanceList.getMinVal(), balanceList.getMaxVal());
  }

  @Override
  @Transactional(readOnly = true)
  public BaseFilterResponse<ContractFilterResponse> getContracts(Pageable pageable) {
    Page<Address> contractPage = addressRepository.findAllByAddressHasScriptIsTrue(pageable);
    Page<ContractFilterResponse> pageResponse = contractPage.map(addressMapper::fromAddressToContractFilter);
    return new BaseFilterResponse<>(pageResponse);
  }

  @Override
  @Transactional(readOnly = true)
  public BaseFilterResponse<AddressFilterResponse> getTopAddress(Pageable pageable) {
    List<Address> addressPage = addressRepository.findAllOrderByBalance(pageable);
    List<AddressFilterResponse> responses = addressPage.stream()
        .map(addressMapper::fromAddressToFilterResponse).collect(Collectors.toList());
    Page<AddressFilterResponse> pageResponse
        = new PageImpl<>(responses, pageable, pageable.getPageSize());
    return new BaseFilterResponse<>(pageResponse);
  }

  /**
   * Get list token by address
   *
   * @param pageable page information
   * @param address  wallet address
   * @return list token by address
   */
  @Transactional(readOnly = true)
  public BaseFilterResponse<TokenAddressResponse> getTokenByAddress(Pageable pageable,
      String address) {

    Address addr = addressRepository.findFirstByAddress(address).orElseThrow(
        () -> new NoContentException(BusinessCode.ADDRESS_NOT_FOUND)
    );

    Page<AddressTokenProjection> addressTokenProjectionPage =
        addressTokenBalanceRepository.findAddressAndBalanceByAddress(addr, pageable);

    List<AddressTokenProjection> addressTokenProjectionList = addressTokenProjectionPage.getContent();
    long totalElements = addressTokenProjectionPage.getTotalElements();

    List<Long> multiAssetIdList = addressTokenProjectionList.stream()
        .map(AddressTokenProjection::getMultiAssetId).collect(Collectors.toList());
    List<MultiAsset> multiAssetList = multiAssetRepository.findAllById(multiAssetIdList);

    Map<Long, MultiAsset> multiAssetMap = multiAssetList.stream()
        .collect(Collectors.toMap(MultiAsset::getId, Function.identity()));

    List<TokenAddressResponse> tokenListResponse = addressTokenProjectionList.stream()
        .map(addressTokenProjection -> {
            MultiAsset multiAsset = multiAssetMap.get(addressTokenProjection.getMultiAssetId());
          return tokenMapper.fromMultiAssetAndAddressToken(multiAsset, addressTokenProjection);
        }).collect(Collectors.toList());

    setMetadata(tokenListResponse);

    Page<TokenAddressResponse> pageResponse = new PageImpl<>(tokenListResponse, pageable,
        totalElements);
    return new BaseFilterResponse<>(pageResponse);
  }

  /**
   * Get list token by display name
   *
   * @param pageable    page information
   * @param address     wallet address
   * @param displayName display name of token
   * @return list token by display name
   */
  @Override
  @Transactional(readOnly = true)
  public BaseFilterResponse<TokenAddressResponse> getTokenByDisplayName(Pageable pageable,
      String address, String displayName) {
    if (StringUtils.isEmpty(displayName)) {
      return getTokenByAddress(pageable, address);
    }

    Address addr = addressRepository.findFirstByAddress(address).orElseThrow(
        () -> new NoContentException(BusinessCode.ADDRESS_NOT_FOUND)
    );

    List<AddressTokenProjection> addressTokenProjectionList =
        addressTokenBalanceRepository.
            findAddressAndBalanceByAddress(addr)
        .stream()
        .filter(addressTokenProjection -> HexUtils.fromHex(addressTokenProjection.getTokenName(),
            addressTokenProjection.getFingerprint()).toLowerCase().contains(displayName.toLowerCase()))
        .collect(Collectors.toList());
    
    List<TokenAddressResponse> tokenListResponse = addressTokenProjectionList.stream()
        .map(tokenMapper::fromAddressTokenProjection)
        .sorted(Comparator.comparing(TokenAddressResponse::getQuantity).reversed()
            .thenComparing(TokenAddressResponse::getDisplayName))
        .collect(Collectors.toList());

    setMetadata(tokenListResponse);

    final int start = (int) pageable.getOffset();
    final int end = Math.min((start + pageable.getPageSize()), addressTokenProjectionList.size());
    Page<TokenAddressResponse> pageResponse = new PageImpl<>(tokenListResponse.subList(start, end),
        pageable, addressTokenProjectionList.size());
    return new BaseFilterResponse<>(pageResponse);
  }

  @Override
  public Boolean verifyNativeScript(ScriptVerifyRequest scriptVerifyRequest) {
    try{
      ShelleyAddress shelleyAddress = new ShelleyAddress(scriptVerifyRequest.getAddress());
      String policyId = shelleyAddress.getHexPaymentPart();
      String hash = Hex.encodeHexString(NativeScript.deserializeJson(scriptVerifyRequest.getScript())
                                            .getScriptHash());
      if(policyId.equals(hash)){
        Address address = addressRepository.findFirstByAddress(scriptVerifyRequest.getAddress())
            .orElseThrow(() -> new NoContentException(BusinessCode.ADDRESS_NOT_FOUND));
        address.setVerifiedContract(Boolean.TRUE);
        addressRepository.save(address);
        return Boolean.TRUE;
      } else{
        return Boolean.FALSE;
      }
    } catch (Exception e) {
      throw new BusinessException(BusinessCode.INTERNAL_ERROR);
    }
  }

  @Override
  public String getJsonNativeScript(String address) {
    Address addr = addressRepository.findFirstByAddress(address).orElseThrow(
        () -> new NoContentException(BusinessCode.ADDRESS_NOT_FOUND)
    );

    if(Boolean.FALSE.equals(addr.getVerifiedContract())){
      return SCRIPT_NOT_VERIFIED;
    }

    ShelleyAddress shelleyAddress = new ShelleyAddress(addr.getAddress());
    String policyId = shelleyAddress.getHexPaymentPart();
    Script script = scriptRepository.findByHash(policyId).orElseThrow(
        () -> new NoContentException(BusinessCode.SCRIPT_NOT_FOUND)
    );

    if(Objects.isNull(script.getJson())){
      return SCRIPT_NOT_VERIFIED;
    }

    return script.getJson();
  }

  private void setMetadata(List<TokenAddressResponse> tokenListResponse) {
    Set<String> subjects = tokenListResponse.stream()
        .map(ma -> ma.getPolicy() + ma.getName()).collect(Collectors.toSet());
    List<AssetMetadata> assetMetadataList = assetMetadataRepository.findBySubjectIn(subjects);
    Map<String, AssetMetadata> assetMetadataMap = assetMetadataList.stream().collect(
        Collectors.toMap(AssetMetadata::getSubject, Function.identity()));

    tokenListResponse.forEach(token -> {
      AssetMetadata assetMetadata = assetMetadataMap.get(token.getPolicy() + token.getName());
      if (Objects.nonNull(assetMetadata)) {
        token.setMetadata(assetMetadataMapper.fromAssetMetadata(assetMetadata));
      }
    });
  }
}
