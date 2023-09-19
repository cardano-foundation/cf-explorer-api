package org.cardanofoundation.explorer.api.service.impl;

import com.bloxbean.cardano.client.transaction.spec.script.NativeScript;
import java.time.LocalDateTime;
import java.util.Optional;
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
import org.cardanofoundation.explorer.api.model.response.contract.ContractScript;
import org.cardanofoundation.explorer.api.model.response.token.TokenAddressResponse;
import org.cardanofoundation.explorer.api.projection.MinMaxProjection;
import org.cardanofoundation.explorer.api.repository.AddressRepository;
import org.cardanofoundation.explorer.api.repository.AddressTokenBalanceRepository;
import org.cardanofoundation.explorer.api.repository.AddressTxBalanceRepository;
import org.cardanofoundation.explorer.api.repository.AggregateAddressTxBalanceRepository;
import org.cardanofoundation.explorer.api.repository.AssetMetadataRepository;
import org.cardanofoundation.explorer.api.repository.ScriptRepository;
import org.cardanofoundation.explorer.api.service.AddressService;
import org.cardanofoundation.explorer.api.util.AddressUtils;
import org.cardanofoundation.explorer.api.util.DataUtil;
import org.cardanofoundation.explorer.api.util.DateUtils;
import org.cardanofoundation.explorer.common.exceptions.BusinessException;
import org.cardanofoundation.explorer.common.exceptions.NoContentException;
import org.cardanofoundation.explorer.consumercommon.entity.Address;
import org.cardanofoundation.explorer.consumercommon.entity.AssetMetadata;

import java.math.BigInteger;
import java.sql.Timestamp;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.cardanofoundation.explorer.consumercommon.entity.Script;
import org.cardanofoundation.explorer.consumercommon.entity.aggregation.AggregateAddressTxBalance;
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
  @Transactional(readOnly = true)
  public AddressResponse getAddressDetail(String address) {
    Address addr = addressRepository.findFirstByAddress(address).orElse(
        Address.builder().address(address).txCount(0L).balance(BigInteger.ZERO).build()
    );
    final int ADDRESS_MIN_LENGTH = 56;
    if(!checkNetworkAddress(address) || address.length() < ADDRESS_MIN_LENGTH) {
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
    if(address.startsWith(CommonConstant.TESTNET_ADDRESS_PREFIX)) {
      return !network.equals(CommonConstant.MAINNET_NETWORK);
    } else {
      return network.equals(CommonConstant.MAINNET_NETWORK);
    }
  }

  @Override
  @Transactional(readOnly = true)
  public List<AddressAnalyticsResponse> getAddressAnalytics(String address, AnalyticType type) {
    Address addr = addressRepository.findFirstByAddress(address)
        .orElseThrow(() -> new NoContentException(BusinessCode.ADDRESS_NOT_FOUND));
    Long txCount = addressTxBalanceRepository.countByAddress(addr);
    if (Long.valueOf(0).equals(txCount)) {
      return List.of();
    }

    List<LocalDateTime> dates = DateUtils.getListDateAnalytic(type);

    List<AddressAnalyticsResponse> responses = new ArrayList<>();
    if (AnalyticType.ONE_DAY.equals(type)) {
      var fromBalance = aggregateAddressTxBalanceRepository.sumBalanceByAddressId(addr.getId(),
          dates.get(0).minusDays(1).toLocalDate()).orElse(BigInteger.ZERO);
      responses.add(new AddressAnalyticsResponse(dates.get(0), fromBalance));
      for (int i = 1; i < dates.size(); i++) {
        Optional<BigInteger> balance = addressTxBalanceRepository
            .getBalanceByAddressAndTime(addr, Timestamp.valueOf(dates.get(i-1)), Timestamp.valueOf(dates.get(i)));
        if (balance.isPresent()) {
          fromBalance = fromBalance.add(balance.get());
        }
        responses.add(new AddressAnalyticsResponse(dates.get(i), fromBalance));
      }
    } else {
      // Remove last date because we will get data of today
      dates.remove(0);
      var fromBalance = aggregateAddressTxBalanceRepository.sumBalanceByAddressId(addr.getId(),
          dates.get(0).minusDays(1).toLocalDate()).orElse(BigInteger.ZERO);
      List<AggregateAddressTxBalance> aggregateAddressTxBalances = aggregateAddressTxBalanceRepository
          .findAllByAddressIdAndDayBetween(addr.getId(), dates.get(0).toLocalDate(),
              dates.get(dates.size() - 1).toLocalDate());

      // Data in aggregate_address_tx_balance save at end of day, but we will display start of day
      // So we need to add 1 day to display correct data
      Map<LocalDate, BigInteger> mapBalance = aggregateAddressTxBalances.stream()
          .collect(Collectors.toMap(balance -> balance.getDay().plusDays(1),
              AggregateAddressTxBalance::getBalance));
      for (LocalDateTime date : dates) {
        if (mapBalance.containsKey(date.toLocalDate())) {
          fromBalance = fromBalance.add(mapBalance.get(date.toLocalDate()));
        }
        responses.add(new AddressAnalyticsResponse(date, fromBalance));
      }
    }
    return responses;
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
    Page<TokenAddressResponse> tokenListResponse;
    Address addr = addressRepository.findFirstByAddress(address).orElseThrow(
        () -> new NoContentException(BusinessCode.ADDRESS_NOT_FOUND)
    );
    if(DataUtil.isNullOrEmpty(displayName)) {
      tokenListResponse = addressTokenBalanceRepository
          .findTokenAndBalanceByAddress(addr, pageable)
          .map(tokenMapper::fromAddressTokenProjection);
    } else {
      displayName = "%" + displayName.trim().toLowerCase() + "%";
      tokenListResponse = addressTokenBalanceRepository
          .findTokenAndBalanceByAddressAndNameView(addr, displayName, pageable)
          .map(tokenMapper::fromAddressTokenProjection);
    }
    setMetadata(tokenListResponse.getContent());
    return new BaseFilterResponse<>(tokenListResponse);
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
            .orElseThrow(() -> new BusinessException(BusinessCode.ADDRESS_NOT_FOUND));
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
  public ContractScript getJsonNativeScript(String address) {
    Address addr = addressRepository.findFirstByAddress(address).orElseThrow(
        () -> new BusinessException(BusinessCode.ADDRESS_NOT_FOUND)
    );

    if(Boolean.FALSE.equals(addr.getVerifiedContract()) || Objects.isNull(addr.getVerifiedContract())){
      return ContractScript.builder().isVerified(Boolean.FALSE).data(null).build();
    }

    ShelleyAddress shelleyAddress = new ShelleyAddress(addr.getAddress());
    String policyId = shelleyAddress.getHexPaymentPart();
    Script script = scriptRepository.findByHash(policyId).orElseThrow(
        () -> new BusinessException(BusinessCode.SCRIPT_NOT_FOUND)
    );

    if(Objects.isNull(script.getJson())){
      return ContractScript.builder().isVerified(Boolean.FALSE).data(null).build();
    }

    return ContractScript.builder().isVerified(Boolean.TRUE).data(script.getJson()).build();
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
