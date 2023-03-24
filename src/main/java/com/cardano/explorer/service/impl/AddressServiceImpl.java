package com.cardano.explorer.service.impl;

import com.cardano.explorer.common.constant.CommonConstant;
import com.cardano.explorer.common.enumeration.AnalyticType;
import com.cardano.explorer.exception.BusinessCode;
import com.cardano.explorer.mapper.AddressMapper;
import com.cardano.explorer.mapper.AssetMetadataMapper;
import com.cardano.explorer.mapper.TokenMapper;
import com.cardano.explorer.model.response.BaseFilterResponse;
import com.cardano.explorer.model.response.address.AddressAnalyticsResponse;
import com.cardano.explorer.model.response.address.AddressFilterResponse;
import com.cardano.explorer.model.response.address.AddressResponse;
import com.cardano.explorer.model.response.contract.ContractFilterResponse;
import com.cardano.explorer.model.response.token.TokenAddressResponse;
import com.cardano.explorer.projection.AddressTokenProjection;
import com.cardano.explorer.repository.AddressRepository;
import com.cardano.explorer.repository.AddressTxBalanceRepository;
import com.cardano.explorer.repository.AssetMetadataRepository;
import com.cardano.explorer.repository.MultiAssetRepository;
import com.cardano.explorer.service.AddressService;
import com.cardano.explorer.util.AddressUtils;
import com.cardano.explorer.util.HexUtils;
import com.sotatek.cardano.common.entity.Address;
import com.sotatek.cardano.common.entity.AssetMetadata;
import com.sotatek.cardano.common.entity.MultiAsset;
import com.sotatek.cardanocommonapi.exceptions.BusinessException;
import java.math.BigInteger;
import java.sql.Timestamp;
import java.time.LocalDate;
import java.time.LocalTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
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
  private final TokenMapper tokenMapper;
  private final AddressMapper addressMapper;
  private final AssetMetadataMapper assetMetadataMapper;
  static final Integer ADDRESS_ANALYTIC_BALANCE_NUMBER = 5;

  @Value("${application.network}")
  private String network;


  @Override
  @Transactional(readOnly = true)
  public AddressResponse getAddressDetail(String address) {
    Address addr = addressRepository.findFirstByAddress(address).orElse(
        Address.builder().address(address).txCount(0L).balance(BigInteger.ZERO).build()
    );
    if(!checkNetworkAddress(address)) {
      throw new BusinessException(BusinessCode.ADDRESS_NOT_FOUND);
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
  public List<AddressAnalyticsResponse> getAddressAnalytics(String address, AnalyticType type) {
    List<AddressAnalyticsResponse> responses = new ArrayList<>();
    Long txCount = addressTxBalanceRepository.countByAddress(address);
    if(Long.valueOf(0).equals(txCount)) {
      return responses;
    }
    LocalDate currentDate = LocalDate.now();
    List<LocalDate> dates = new ArrayList<>();
    Calendar calendar = Calendar.getInstance();
    switch (type) {
      case ONE_WEEK:
        var currentWeek = LocalDate.ofInstant(calendar.toInstant(),
            calendar.getTimeZone().toZoneId());
        for (int i = ADDRESS_ANALYTIC_BALANCE_NUMBER - 1; i >=0 ; i--) {
          dates.add(currentWeek.minusWeeks(i));
        }
        break;
      case ONE_MONTH:
        var currentMonth = LocalDate.ofInstant(calendar.toInstant(),
            calendar.getTimeZone().toZoneId());
        for (int i = ADDRESS_ANALYTIC_BALANCE_NUMBER - 1; i >=0 ; i--) {
          dates.add(currentMonth.minusMonths(i));
        }
        break;
      case THREE_MONTH:
        for (int i = ADDRESS_ANALYTIC_BALANCE_NUMBER - 1; i >=0 ; i--) {
          dates.add(currentDate.minusMonths(i * 3L));
        }
        break;
      default:
        for (int i = ADDRESS_ANALYTIC_BALANCE_NUMBER - 1; i >=0 ; i--) {
          dates.add(currentDate.minusDays(i));
        }
    }
    dates.forEach(
        item -> {
          AddressAnalyticsResponse response = new AddressAnalyticsResponse();
          var balance = addressTxBalanceRepository.getBalanceByAddressAndTime(address,
              Timestamp.valueOf(item.atTime(LocalTime.MAX)));
          if(Objects.isNull(balance)) {
            response.setValue(BigInteger.ZERO);
          } else {
            response.setValue(balance);
          }
          response.setDate(item);
          responses.add(response);
        }
    );
    return responses;
  }

  @Override
  @Transactional(readOnly = true)
  public List<BigInteger> getAddressMinMaxBalance(String address) {
    List<BigInteger> balanceList = addressTxBalanceRepository.findAllByAddress(address);
    if(balanceList.isEmpty()) {
      return new ArrayList<>();
    }
    BigInteger maxBalance = balanceList.get(0);
    BigInteger minBalance = balanceList.get(0);
    BigInteger sumBalance = balanceList.get(0);
    balanceList.remove(0);
    for(BigInteger balance : balanceList) {
      sumBalance = sumBalance.add(balance);
      if(sumBalance.compareTo(maxBalance) > 0) {
        maxBalance = sumBalance;
      }
      if(sumBalance.compareTo(minBalance) < 0) {
        minBalance = sumBalance;
      }
    }
    return Arrays.asList(minBalance, maxBalance);
  }


  @Override
  @Transactional(readOnly = true)
  public BaseFilterResponse<ContractFilterResponse> getContracts(Pageable pageable) {
    List<Address> contractPage = addressRepository.findAllByAddressHasScriptIsTrue(pageable);
    List<ContractFilterResponse> responses = contractPage.stream()
        .map(addressMapper::fromAddressToContractFilter).collect(Collectors.toList());
    Page<ContractFilterResponse> pageResponse
        = new PageImpl<>(responses, pageable, pageable.getPageSize());
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
  @Override
  @Transactional(readOnly = true)
  public BaseFilterResponse<TokenAddressResponse> getTokenByAddress(Pageable pageable,
      String address) {

    Address addr = addressRepository.findFirstByAddress(address).orElseThrow(
        () -> new BusinessException(BusinessCode.ADDRESS_NOT_FOUND)
    );

    List<AddressTokenProjection> addressTokenProjectionList = multiAssetRepository.getIdentListByAddress(
        addr, pageable).stream().collect(Collectors.toList());
    List<Long> multiAssetIdList = addressTokenProjectionList.stream()
        .map(AddressTokenProjection::getMultiAssetId).collect(Collectors.toList());
    List<MultiAsset> multiAssetList = multiAssetRepository.findAllById(multiAssetIdList);

    Map<Long, AddressTokenProjection> addressTokenProjectionMap = addressTokenProjectionList.stream()
        .collect(Collectors.toMap(AddressTokenProjection::getMultiAssetId, Function.identity()));

    List<TokenAddressResponse> tokenListResponse = multiAssetList.stream()
        .map(multiAsset -> {
          AddressTokenProjection addressTokenProjection = addressTokenProjectionMap.get(
              multiAsset.getId());
          return tokenMapper.fromMultiAssetAndAddressToken(multiAsset, addressTokenProjection);
        }).collect(Collectors.toList());

    Set<String> subjects = tokenListResponse.stream()
        .map(ma -> ma.getPolicy() + ma.getName()).collect(Collectors.toSet());
    List<AssetMetadata> assetMetadataList = assetMetadataRepository.findBySubjectIn(subjects);
    Map<String, AssetMetadata> assetMetadataMap = assetMetadataList.stream().collect(
        Collectors.toMap(AssetMetadata::getSubject, Function.identity()));
    for (int i = 0; i < tokenListResponse.size(); i++) {
      TokenAddressResponse token = tokenListResponse.get(i);
      AssetMetadata assetMetadata = assetMetadataMap.get(token.getPolicy() + token.getName());
      if (Objects.nonNull(assetMetadata)) {
        token.setMetadata(assetMetadataMapper.fromAssetMetadata(assetMetadata));
      }
    }

    Page<TokenAddressResponse> pageResponse = new PageImpl<>(tokenListResponse, pageable,
        pageable.getPageSize());
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
        () -> new BusinessException(BusinessCode.ADDRESS_NOT_FOUND)
    );

    List<AddressTokenProjection> addressTokenProjectionList = multiAssetRepository.getIdentListByAddress(
        addr);
    List<Long> multiAssetIdList = addressTokenProjectionList.stream()
        .map(AddressTokenProjection::getMultiAssetId).collect(Collectors.toList());

    List<MultiAsset> multiAssetList = multiAssetRepository.findAllById(multiAssetIdList).stream()
        .filter(multiAsset -> HexUtils.fromHex(multiAsset.getName().toLowerCase(),
            multiAsset.getFingerprint()).contains(displayName.toLowerCase()))
        .collect(Collectors.toList());

    Map<Long, AddressTokenProjection> addressTokenProjectionMap = addressTokenProjectionList.stream()
        .collect(Collectors.toMap(AddressTokenProjection::getMultiAssetId, Function.identity()));

    List<TokenAddressResponse> tokenListResponse = multiAssetList.stream()
        .map(multiAsset -> {
          AddressTokenProjection addressTokenProjection = addressTokenProjectionMap.get(
              multiAsset.getId());
          return tokenMapper.fromMultiAssetAndAddressToken(multiAsset, addressTokenProjection);
        })
        .sorted(Comparator.comparing(TokenAddressResponse::getAddress))
        .collect(Collectors.toList());

    final int start = (int) pageable.getOffset();
    final int end = Math.min((start + pageable.getPageSize()), tokenListResponse.size());
    Page<TokenAddressResponse> pageResponse = new PageImpl<>(tokenListResponse.subList(start, end),
        pageable, tokenListResponse.size());
    return new BaseFilterResponse<>(pageResponse);
  }
}
