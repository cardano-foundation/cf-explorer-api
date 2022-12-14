package com.cardano.explorer.service.impl;

import com.cardano.explorer.common.enumeration.AnalyticType;
import com.cardano.explorer.exception.BusinessCode;
import com.cardano.explorer.mapper.TokenMapper;
import com.cardano.explorer.model.response.address.AddressAnalyticsResponse;
import com.cardano.explorer.model.response.address.AddressResponse;
import com.cardano.explorer.repository.AddressTxBalanceRepository;
import com.cardano.explorer.repository.MultiAssetRepository;
import com.cardano.explorer.repository.custom.impl.CustomAddressTxBalanceRepositoryImpl;
import com.cardano.explorer.service.AddressService;
import com.sotatek.cardano.ledgersync.common.address.ShelleyAddress;
import com.sotatek.cardanocommonapi.exceptions.BusinessException;
import java.math.BigDecimal;
import java.sql.Timestamp;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@RequiredArgsConstructor
public class AddressServiceImpl implements AddressService {

  private final MultiAssetRepository multiAssetRepository;

  private final AddressTxBalanceRepository addressTxBalanceRepository;
  private final CustomAddressTxBalanceRepositoryImpl customAddressTxBalanceRepository;
  private final TokenMapper tokenMapper;
  static final Integer ADDRESS_ANALYTIC_BALANCE_NUMBER = 5;

  @Override
  @Transactional(readOnly = true)
  public AddressResponse getAddressDetail(String address) {
    Integer txCount = addressTxBalanceRepository.countByAddress(address);
    AddressResponse addressResponse = new AddressResponse();
    try {
      ShelleyAddress shelleyAddress = new ShelleyAddress(address);
      if(shelleyAddress.containStakeAddress()){
        //TO-DO: Move to common
        byte[] addr = shelleyAddress.getStakeReference();
        ShelleyAddress stakeShelley = new ShelleyAddress(addr);
        addressResponse.setStakeAddress(stakeShelley.getAddress());
      } else {
        throw new BusinessException(BusinessCode.ADDRESS_NOT_FOUND);
      }
    } catch (Exception e) {
      if (txCount.equals(0)) {
        throw new BusinessException(BusinessCode.ADDRESS_NOT_FOUND);
      }
    }
    addressResponse.setAddress(address);
    var balance = customAddressTxBalanceRepository.getBalanceByAddressAndTime(address,
        Timestamp.valueOf(LocalDateTime.now()));
    addressResponse.setBalance(balance);
    addressResponse.setTxCount(txCount);
    addressResponse.setTokens(multiAssetRepository.findTokenByAddress(address).stream().map(
        tokenMapper::fromAddressTokenProjection
    ).collect(Collectors.toList()));
    return addressResponse;

  }

  @Override
  @Transactional(readOnly = true)
  public List<AddressAnalyticsResponse> getAddressAnalytics(String address, AnalyticType type) {
    List<AddressAnalyticsResponse> responses = new ArrayList<>();
    LocalDate currentDate = LocalDate.now();
    List<LocalDate> dates = new ArrayList<>();
    Calendar calendar = Calendar.getInstance();
    switch (type) {
      case ONE_WEEK:
        var currentWeek = LocalDate.ofInstant(calendar.toInstant(),
            calendar.getTimeZone().toZoneId());
        for (int i = 0; i < ADDRESS_ANALYTIC_BALANCE_NUMBER; i++) {
          dates.add(currentWeek.minusWeeks(i));
        }
        break;
      case ONE_MONTH:
        var currentMonth = LocalDate.ofInstant(calendar.toInstant(),
            calendar.getTimeZone().toZoneId());
        for (int i = 0; i < ADDRESS_ANALYTIC_BALANCE_NUMBER; i++) {
          dates.add(currentMonth.minusMonths(i));
        }
        break;
      case THREE_MONTH:
        for (int i = 0; i < ADDRESS_ANALYTIC_BALANCE_NUMBER; i++) {
          dates.add(currentDate.minusMonths(i * 3L));
        }
        break;
      default:
        for (int i = 0; i < ADDRESS_ANALYTIC_BALANCE_NUMBER; i++) {
          dates.add(currentDate.minusDays(i));
        }
    }
    dates.forEach(
        item -> {
          AddressAnalyticsResponse response = new AddressAnalyticsResponse();
          var balance = customAddressTxBalanceRepository.getBalanceByAddressAndTime(address,
              Timestamp.valueOf(item.atTime(LocalTime.MAX)));
          if(Objects.isNull(balance)) {
            return;
          }
          response.setDate(item);
          response.setValue(balance);
          responses.add(response);
        }
    );
    return responses;
  }

  @Override
  public BigDecimal getAddressMaxBalance(String address) {
    return addressTxBalanceRepository.getMaxBalanceByAddress(address);
  }


  @Override
  public BigDecimal getAddressMinBalance(String address) {
    return addressTxBalanceRepository.getMinBalanceByAddress(address);
  }
}
