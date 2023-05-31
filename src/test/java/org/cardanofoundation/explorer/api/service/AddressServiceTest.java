package org.cardanofoundation.explorer.api.service;

import java.util.Optional;

import org.cardanofoundation.explorer.api.model.request.ScriptVerifyRequest;
import org.cardanofoundation.explorer.api.repository.AddressRepository;
import org.cardanofoundation.explorer.api.repository.ScriptRepository;
import org.cardanofoundation.explorer.api.service.impl.AddressServiceImpl;
import org.cardanofoundation.explorer.common.exceptions.BusinessException;
import org.cardanofoundation.explorer.consumercommon.entity.Address;
import org.cardanofoundation.explorer.consumercommon.entity.Script;
import org.cardanofoundation.ledgersync.common.common.address.ShelleyAddress;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;


import static org.mockito.Mockito.any;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

@ExtendWith(MockitoExtension.class)
public class AddressServiceTest {
  static final String SCRIPT_NOT_VERIFIED = "Script not verified";

  @Mock
  AddressRepository addressRepository;
  @Mock
  ScriptRepository scriptRepository;

  @InjectMocks
  AddressServiceImpl addressService;

  @Test
  void verifyNativeScript_shouldReturnTrueIfPolicyIdMatches() {

    ScriptVerifyRequest scriptVerifyRequest = ScriptVerifyRequest.builder()
        .address("addr1zy6ndumcmaesy7wj86k8jwup0vn5vewklc6jxlrrxr5tjqda8awvzhtzntme2azmkacmvtc4ggrudqxcmyl245nq5taq6yclrm")
        .script("{\"type\":\"all\",\"scripts\":[{\"type\":\"sig\",\"keyHash\":\"e3124f98d11157535bc61ce8db0e04e95cfcf552a86bb116e593a76e\"},{\"type\":\"sig\",\"keyHash\":\"e52515f0f5e25adb0c57ac5835f67bf703e10e494be391d4b41bcfbd\"}]}")
        .build();

    Address address = Address.builder()
        .address("addr1zy6ndumcmaesy7wj86k8jwup0vn5vewklc6jxlrrxr5tjqda8awvzhtzntme2azmkacmvtc4ggrudqxcmyl245nq5taq6yclrm")
        .verifiedContract(false)
        .build();

    when(addressRepository.findFirstByAddress(scriptVerifyRequest.getAddress()))
        .thenReturn(Optional.of(address));
    when(addressRepository.save(any(Address.class))).thenReturn(new Address());
    Assertions.assertTrue(addressService.verifyNativeScript(scriptVerifyRequest));
  }

  @Test
  void verifyNativeScript_shouldReturnFalseIfPolicyIdDoesNotMatch() {

    ScriptVerifyRequest scriptVerifyRequest = ScriptVerifyRequest.builder()
        .address("addr1zy6ndumcmaesy7wj86k8jwup0vn5vewklc6jxlrrxr5tjqda8awvzhtzntme2azmkacmvtc4ggrudqxcmyl245nq5taq6yclrm")
        .script("{\"type\":\"all\",\"scripts\":[{\"type\":\"sig\",\"keyHash\":\"e3124f97d11157535bc61ce8db0e04e95cfcf552a86bb116e593a76e\"},{\"type\":\"sig\",\"keyHash\":\"e52515f0f5e25adb0c57ac5835f67bf703e10e494be391d4b41bcfbd\"}]}")
        .build();

    Assertions.assertFalse(addressService.verifyNativeScript(scriptVerifyRequest));
  }

  @Test
  void verifyNativeScript_shouldThrowException_whenAnyErrorsOccur() {

    ScriptVerifyRequest scriptVerifyRequest = ScriptVerifyRequest.builder()
        .address("addr1zy6ndumcmaesy7wj86k8jwup0vn5vewklc6jxlrrxr5tjqda8awvzhtzntme2azmkacmvtc4ggrudqxcmyl245nq5taq6yclrm")
        .script("Json parse error")
        .build();

    Assertions.assertThrows(BusinessException.class, () -> addressService.verifyNativeScript(scriptVerifyRequest));
  }

  @Test
  void getJsonNativeScript_shouldReturnScripNotVerify_whenContractNotVerifyYet(){
    Address address = Address.builder()
        .address("addr1zy6ndumcmaesy7wj86k8jwup0vn5vewklc6jxlrrxr5tjqda8awvzhtzntme2azmkacmvtc4ggrudqxcmyl245nq5taq6yclrm")
        .verifiedContract(false)
        .build();

    when(addressRepository.findFirstByAddress(address.getAddress()))
        .thenReturn(Optional.of(address));

    Assertions.assertEquals(addressService.getJsonNativeScript(address.getAddress()), SCRIPT_NOT_VERIFIED);
  }

  @Test
  void getJsonNativeScript_shouldReturnNativeJsonScript() {
    Address address = Address.builder()
        .address(
            "addr1zy6ndumcmaesy7wj86k8jwup0vn5vewklc6jxlrrxr5tjqda8awvzhtzntme2azmkacmvtc4ggrudqxcmyl245nq5taq6yclrm")
        .verifiedContract(true)
        .build();

    ShelleyAddress shelleyAddress = new ShelleyAddress(address.getAddress());
    String policyId = shelleyAddress.getHexPaymentPart();
    Script script = Script.builder()
        .json("native script")
        .build();

    when(addressRepository.findFirstByAddress(address.getAddress()))
        .thenReturn(Optional.of(address));
    when(scriptRepository.findByHash(policyId))
        .thenReturn(Optional.of(script));

    Assertions.assertEquals(addressService.getJsonNativeScript(address.getAddress()),
                            script.getJson());
  }

  @Test
  void getJsonNativeScript_shouldReturnScripNotVerify_whenNativeScripJsonNull() {
    Address address = Address.builder()
        .address(
            "addr1zy6ndumcmaesy7wj86k8jwup0vn5vewklc6jxlrrxr5tjqda8awvzhtzntme2azmkacmvtc4ggrudqxcmyl245nq5taq6yclrm")
        .verifiedContract(true)
        .build();

    ShelleyAddress shelleyAddress = new ShelleyAddress(address.getAddress());
    String policyId = shelleyAddress.getHexPaymentPart();
    Script script = Script.builder()
        .build();

    when(addressRepository.findFirstByAddress(address.getAddress()))
        .thenReturn(Optional.of(address));
    when(scriptRepository.findByHash(policyId))
        .thenReturn(Optional.of(script));

    Assertions.assertEquals(addressService.getJsonNativeScript(address.getAddress()),
                            SCRIPT_NOT_VERIFIED);
  }
}
