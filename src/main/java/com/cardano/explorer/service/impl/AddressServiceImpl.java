package com.cardano.explorer.service.impl;

import com.bloxbean.cardano.client.address.Address;
import com.cardano.explorer.exception.BusinessCode;
import com.cardano.explorer.model.response.address.AddressAnalyticsResponse;
import com.cardano.explorer.model.response.address.AddressResponse;
import com.cardano.explorer.repository.TxOutRepository;
import com.cardano.explorer.service.AddressService;
import com.sotatek.cardano.ledgersync.util.AddressUtil;
import com.sotatek.cardanocommonapi.exceptions.BusinessException;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
public class AddressServiceImpl implements AddressService {

  private final TxOutRepository txOutRepository;

  @Override
  public AddressResponse getAddressDetail(String address) {
    Integer txCount = txOutRepository.countByAddress(address);
    AddressResponse addressResponse = new AddressResponse();
    try {
      Address stake = AddressUtil.baseAddressToStakeAddress(address);
      addressResponse.setStakeAddress(stake.getAddress());
    } catch (Exception e) {
      if(txCount == 0) {
        throw new BusinessException(BusinessCode.ADDRESS_NOT_FOUND);
      }
    }
    BigDecimal totalOutput = txOutRepository.getAddressTotalOutput(address).orElse(BigDecimal.ZERO);
    BigDecimal totalInput = txOutRepository.getAddressTotalInput(address).orElse(BigDecimal.ZERO);
    addressResponse.setAddress(address);
    addressResponse.setBalance(totalOutput.subtract(totalInput));
    addressResponse.setTxCount(txCount);
    return addressResponse;

  }

  @Override
  public List<AddressAnalyticsResponse> getAddressAnalytics(String address, String type) {
    List<AddressAnalyticsResponse> responses = new ArrayList<>();
    //TO-DO
    return responses;
  }
}
