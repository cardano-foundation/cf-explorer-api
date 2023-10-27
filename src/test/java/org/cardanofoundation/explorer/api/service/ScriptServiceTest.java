package org.cardanofoundation.explorer.api.service;

import org.cardanofoundation.explorer.api.projection.PolicyProjection;
import org.cardanofoundation.explorer.api.projection.SmartContractProjection;
import org.cardanofoundation.explorer.api.repository.MultiAssetRepository;
import org.cardanofoundation.explorer.api.repository.ScriptRepository;
import org.cardanofoundation.explorer.api.repository.StakeAddressRepository;
import org.cardanofoundation.explorer.api.repository.TxOutRepository;
import org.cardanofoundation.explorer.api.service.impl.ScriptServiceImpl;
import org.cardanofoundation.explorer.consumercommon.entity.Script;
import org.cardanofoundation.explorer.consumercommon.enumeration.ScriptType;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;

import java.util.ArrayList;
import java.util.List;

import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
public class ScriptServiceTest {
  @InjectMocks private ScriptServiceImpl scriptService;
  @Mock private ScriptRepository scriptRepository;
  @Mock private MultiAssetRepository multiAssetRepository;
  @Mock private TxOutRepository txOutRepository;
  @Mock private StakeAddressRepository stakeAddressRepository;

  @Test
  public void testGetNativeScripts() {
    // Given
    Pageable pageable = PageRequest.of(0, 1);
    List<ScriptType> nativeScriptTypes = List.of(ScriptType.TIMELOCK, ScriptType.MULTISIG);
    List<Script> scriptList = List.of(Script.builder().hash("hash").type(ScriptType.TIMELOCK).id(1L).build());
    PolicyProjection projection = Mockito.mock(PolicyProjection.class);
    // When and Then
    when(projection.getPolicy()).thenReturn("hash");
    when(projection.getNumberOfTokens()).thenReturn(2);
    when(projection.getNumberOfAssetHolders()).thenReturn(3);
    when(scriptRepository.findAllByTypeIn(nativeScriptTypes, pageable))
        .thenReturn(new PageImpl<>(scriptList));
    when(multiAssetRepository.countMultiAssetByPolicyIn(List.of("hash")))
        .thenReturn(List.of(projection));
    when(multiAssetRepository.countAssetHoldersByPolicyIn(List.of("hash")))
        .thenReturn(List.of(projection));
    var response = scriptService.getNativeScripts(pageable);
    // Assert
    Assertions.assertEquals(1, response.getTotalItems());
    Assertions.assertEquals(1, response.getTotalPages());
    Assertions.assertEquals(1, response.getData().size());
    Assertions.assertEquals("hash", response.getData().get(0).getScriptHash());
    Assertions.assertEquals(2, response.getData().get(0).getNumberOfTokens());
    Assertions.assertEquals(3, response.getData().get(0).getNumberOfAssetHolders());
  }

  @Test
  public void testGetSmartContracts() {
    // Given
    Pageable pageable = PageRequest.of(0, 1);
    List<ScriptType> nativeScriptTypes = List.of(ScriptType.PLUTUSV1, ScriptType.PLUTUSV2);
    List<Script> scriptList = List.of(Script.builder().hash("hash").type(ScriptType.PLUTUSV1).id(1L).build());
    SmartContractProjection paymentAddressProjection = Mockito.mock(SmartContractProjection.class);
    SmartContractProjection stakeAddressProjection = Mockito.mock(SmartContractProjection.class);
    // When and Then
    when(paymentAddressProjection.getScriptHash()).thenReturn("hash");
    when(paymentAddressProjection.getAddress()).thenReturn("address");
    when(stakeAddressProjection.getScriptHash()).thenReturn("hash");
    when(stakeAddressProjection.getAddress()).thenReturn("stake");
    when(scriptRepository.findAllByTypeIn(nativeScriptTypes, pageable))
        .thenReturn(new PageImpl<>(scriptList));
    when(txOutRepository.findPaymentAssociatedAddressByHashIn(List.of("hash")))
        .thenReturn(List.of(paymentAddressProjection));
    when(stakeAddressRepository.findStakeAssociatedAddressByHashIn(List.of("hash")))
        .thenReturn(List.of(stakeAddressProjection));
    var response = scriptService.getSmartContracts(pageable);
    // Assert
    Assertions.assertEquals(1, response.getTotalItems());
    Assertions.assertEquals(1, response.getTotalPages());
    Assertions.assertEquals(1, response.getData().size());
    Assertions.assertEquals("hash", response.getData().get(0).getScriptHash());
    Assertions.assertEquals(2, response.getData().get(0).getAssociatedAddress().size());
    Assertions.assertEquals("stake", response.getData().get(0).getAssociatedAddress().get(0));
    Assertions.assertEquals("address", response.getData().get(0).getAssociatedAddress().get(1));
  }

}
