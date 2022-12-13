package com.cardano.explorer.service.impl;

import com.bloxbean.cardano.client.address.Address;
import com.cardano.explorer.common.enumeration.AnalyticType;
import com.cardano.explorer.exception.BusinessCode;
import com.cardano.explorer.mapper.TokenMapper;
import com.cardano.explorer.model.response.address.AddressAnalyticsResponse;
import com.cardano.explorer.model.response.address.AddressResponse;
import com.cardano.explorer.repository.AddressTxBalanceRepository;
import com.cardano.explorer.repository.MultiAssetRepository;
import com.cardano.explorer.service.AddressService;
import com.sotatek.cardano.ledgersync.util.AddressUtil;
import com.sotatek.cardanocommonapi.exceptions.BusinessException;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@RequiredArgsConstructor
public class AddressServiceImpl implements AddressService {

  private final MultiAssetRepository multiAssetRepository;

  private final AddressTxBalanceRepository addressTxBalanceRepository;
  private final TokenMapper tokenMapper;

  @Override
  @Transactional(readOnly = true)
  public AddressResponse getAddressDetail(String address) {
    Integer txCount = addressTxBalanceRepository.countByAddress(address);
    AddressResponse addressResponse = new AddressResponse();
    try {
      Address stake = AddressUtil.baseAddressToStakeAddress(address);
      addressResponse.setStakeAddress(stake.getAddress());
    } catch (Exception e) {
      if (txCount.equals(0)) {
        throw new BusinessException(BusinessCode.ADDRESS_NOT_FOUND);
      }
    }
    addressResponse.setAddress(address);
    addressResponse.setBalance(addressTxBalanceRepository.getBalanceByAddress(address));
    addressResponse.setTxCount(txCount);
    addressResponse.setTokens(multiAssetRepository.findTokenByAddress(address).stream().map(
        tokenMapper::fromAddressTokenProjection
    ).collect(Collectors.toList()));
    return addressResponse;

  }

  @Override
  public List<AddressAnalyticsResponse> getAddressAnalytics(String address, AnalyticType type) {
    List<AddressAnalyticsResponse> responses = new ArrayList<>();
    //TO-DO
    return responses;
  }
}
