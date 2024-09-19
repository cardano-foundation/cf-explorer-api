package org.cardanofoundation.explorer.api.service.impl;

import java.util.Objects;
import java.util.Optional;

import lombok.RequiredArgsConstructor;

import org.springframework.stereotype.Service;

import org.cardanofoundation.explorer.api.common.constant.CommonConstant;
import org.cardanofoundation.explorer.api.model.response.micar.AddressCarbonEmissionResponse;
import org.cardanofoundation.explorer.api.repository.ledgersync.StakeAddressRepository;
import org.cardanofoundation.explorer.api.repository.ledgersyncagg.AddressRepository;
import org.cardanofoundation.explorer.api.repository.ledgersyncagg.AddressTxCountRepository;
import org.cardanofoundation.explorer.api.repository.ledgersyncagg.StakeAddressTxCountRepository;
import org.cardanofoundation.explorer.api.service.MiCARService;
import org.cardanofoundation.explorer.api.util.AddressUtils;
import org.cardanofoundation.explorer.common.entity.ledgersync.StakeAddress;
import org.cardanofoundation.explorer.common.entity.ledgersyncsagg.Address;
import org.cardanofoundation.explorer.common.entity.ledgersyncsagg.AddressTxCount;
import org.cardanofoundation.explorer.common.entity.ledgersyncsagg.StakeAddressTxCount;

@Service
@RequiredArgsConstructor
public class MiCARServiceImpl implements MiCARService {
  private final StakeAddressTxCountRepository stakeAddressTxCountRepository;
  private final AddressTxCountRepository addressTxCountRepository;
  private final StakeAddressRepository stakeAddressRepository;
  private final AddressRepository addressRepository;

  @Override
  public AddressCarbonEmissionResponse getCarbonEmissionsByAddressAndPool(String address) {
    if (Objects.isNull(address)) {
      return AddressCarbonEmissionResponse.builder().build();
    }
    if (address.startsWith(CommonConstant.PREFIXED_STAKE_KEY)) {
      Optional<StakeAddress> stakeAddress = stakeAddressRepository.findByView(address);
      if (stakeAddress.isEmpty()) {
        return AddressCarbonEmissionResponse.builder().build();
      }
      Optional<StakeAddressTxCount> stakeAddressTxCount =
          stakeAddressTxCountRepository.findByStakeAddress(stakeAddress.get().getView());
      return AddressCarbonEmissionResponse.builder()
          .stakeAddress(address)
          .txCount(stakeAddressTxCount.orElseGet(StakeAddressTxCount::new).getTxCount())
          .carbonEmissionPerTx(CommonConstant.MiCAR.CO2_EMISSION_PER_TX)
          .build();
    } else {
      try {
        AddressUtils.checkStakeAddress(address);
        Optional<Address> addr = addressRepository.findFirstByAddress(address);
        if (addr.isEmpty()) {
          return AddressCarbonEmissionResponse.builder().build();
        }
        Optional<AddressTxCount> addressTxCount = addressTxCountRepository.findByAddress(address);
        return AddressCarbonEmissionResponse.builder()
            .address(address)
            .txCount(addressTxCount.orElseGet(AddressTxCount::new).getTxCount())
            .carbonEmissionPerTx(CommonConstant.MiCAR.CO2_EMISSION_PER_TX)
            .build();
      } catch (Exception e) {
        return AddressCarbonEmissionResponse.builder().build();
      }
    }
  }
}
