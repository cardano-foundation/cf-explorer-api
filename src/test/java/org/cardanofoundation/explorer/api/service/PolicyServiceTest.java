package org.cardanofoundation.explorer.api.service;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import java.util.List;
import java.util.Optional;

import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import org.cardanofoundation.explorer.api.mapper.AssetMetadataMapper;
import org.cardanofoundation.explorer.api.mapper.TokenMapper;
import org.cardanofoundation.explorer.api.model.response.token.TokenFilterResponse;
import org.cardanofoundation.explorer.api.model.response.token.TokenMetadataResponse;
import org.cardanofoundation.explorer.api.repository.ledgersync.AssetMetadataRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.MultiAssetRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.ScriptRepository;
import org.cardanofoundation.explorer.api.repository.ledgersyncagg.AddressRepository;
import org.cardanofoundation.explorer.api.service.impl.PolicyServiceImpl;
import org.cardanofoundation.explorer.common.entity.ledgersync.AssetMetadata;
import org.cardanofoundation.explorer.common.entity.ledgersync.MultiAsset;
import org.cardanofoundation.explorer.common.entity.ledgersync.Script;
import org.cardanofoundation.explorer.common.exception.BusinessException;

@ExtendWith(MockitoExtension.class)
public class PolicyServiceTest {
  @Mock private MultiAssetRepository multiAssetRepository;
  @Mock private AssetMetadataRepository assetMetadataRepository;
  @Mock private AddressRepository addressRepository;
  @Mock private AssetMetadataMapper assetMetadataMapper;
  @Mock private TokenMapper tokenMapper;
  @Mock private ScriptRepository scriptRepository;
  @Mock private ObjectMapper objectMapper;
  @InjectMocks private PolicyServiceImpl policyService;

  @Test
  void testGetPolicyDetail_throwPolicyNotFound() {
    String policyId = "1";

    when(multiAssetRepository.countByPolicy(policyId)).thenReturn(0);

    assertThrows(BusinessException.class, () -> policyService.getPolicyDetail(policyId));
  }

  @Test
  void testGetPolicyDetail_thenReturn() throws JsonProcessingException {
    String policyId = "1";

    when(multiAssetRepository.countByPolicy(policyId)).thenReturn(1);
    when(scriptRepository.findByHash(policyId))
        .thenReturn(Optional.of(Script.builder().bytes(new byte[0]).build()));
    when(objectMapper.writeValueAsString(any())).thenReturn("policy");

    var response = policyService.getPolicyDetail(policyId);
    assertEquals(response.getPolicyId(), "1");
    assertEquals(response.getTotalToken(), 1);
    assertEquals(response.getPolicyScript(), "policy");
  }

  @Test
  void testGetPolicyDetail_thenReturnScriptExist() {
    String policyId = "1";

    when(multiAssetRepository.countByPolicy(policyId)).thenReturn(1);
    when(scriptRepository.findByHash(policyId))
        .thenReturn(Optional.of(Script.builder().json("policy").build()));

    var response = policyService.getPolicyDetail(policyId);
    assertEquals(response.getPolicyId(), "1");
    assertEquals(response.getTotalToken(), 1);
    assertEquals(response.getPolicyScript(), "policy");
  }

  @Test
  void testGetPolicyDetail_throwObjectMapper() throws JsonProcessingException {
    String policyId = "1";

    when(multiAssetRepository.countByPolicy(policyId)).thenReturn(1);
    when(scriptRepository.findByHash(policyId))
        .thenReturn(Optional.of(Script.builder().bytes(new byte[0]).build()));
    when(objectMapper.writeValueAsString(any())).thenThrow(JsonProcessingException.class);

    var response = policyService.getPolicyDetail(policyId);
    assertEquals(response.getPolicyId(), "1");
    assertEquals(response.getTotalToken(), 1);
    assertNull(response.getPolicyScript());
  }

  @Test
  void testGetTokens_thenReturn() {
    String policyId = "1";
    Pageable pageable = PageRequest.of(0, 10);

    when(multiAssetRepository.findAllByPolicy(policyId, pageable))
        .thenReturn(new PageImpl<>(List.of(MultiAsset.builder().policy("1").name("name").build())));
    when(assetMetadataRepository.findBySubjectIn(any()))
        .thenReturn(List.of(AssetMetadata.builder().subject("subject").build()));
    when(tokenMapper.fromMultiAssetToFilterResponse(any()))
        .thenReturn(TokenFilterResponse.builder().policy("1").name("name").build());
    when(assetMetadataMapper.fromAssetMetadata(any()))
        .thenReturn(
            TokenMetadataResponse.builder()
                .logo("logo")
                .decimals(1)
                .description("description")
                .url("url")
                .build());

    var response = policyService.getTokens(policyId, pageable);
    assertEquals(response.getTotalItems(), 1);
    assertEquals(response.getTotalPages(), 1);
    assertEquals(response.getCurrentPage(), 0);
    assertEquals(response.getData().get(0).getName(), "name");
    assertEquals(response.getData().get(0).getPolicy(), "1");
    assertEquals(response.getData().get(0).getMetadata().getUrl(), "url");
    assertEquals(response.getData().get(0).getMetadata().getLogo(), "logo");
    assertEquals(response.getData().get(0).getMetadata().getDescription(), "description");
  }
}
