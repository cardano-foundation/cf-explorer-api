package com.cardano.explorer.service.impl;

import com.cardano.explorer.common.enumeration.AnalyticType;
import com.cardano.explorer.exception.BusinessCode;
import com.cardano.explorer.mapper.TokenMapper;
import com.cardano.explorer.model.response.address.AddressAnalyticsResponse;
import com.cardano.explorer.model.response.address.AddressResponse;
import com.cardano.explorer.repository.AddressTxBalanceRepository;
import com.cardano.explorer.repository.MultiAssetRepository;
import com.cardano.explorer.service.AddressService;
import com.sotatek.cardano.ledgersync.common.address.ShelleyAddress;
import com.sotatek.cardanocommonapi.exceptions.BusinessException;
import java.math.BigDecimal;
import java.sql.Timestamp;
import java.time.LocalDate;
import java.time.LocalTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@RequiredArgsConstructor
@Log4j2
public class AddressServiceImpl implements AddressService {

  private final MultiAssetRepository multiAssetRepository;

  private final AddressTxBalanceRepository addressTxBalanceRepository;
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
    var currentBalance = addressTxBalanceRepository.getBalanceByAddressAndTime(address,
        Timestamp.valueOf(LocalDate.now().atTime(LocalTime.MAX)));
    addressResponse.setBalance(currentBalance);
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
            response.setValue(BigDecimal.ZERO);
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
  public List<BigDecimal> getAddressMinMaxBalance(String address) {
    List<BigDecimal> balanceList = addressTxBalanceRepository.findAllByAddress(address);
    BigDecimal maxBalance = balanceList.get(0);
    BigDecimal minBalance = balanceList.get(0);
    BigDecimal sumBalance = balanceList.get(0);
    balanceList.remove(0);
    for(BigDecimal balance : balanceList) {
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
}
