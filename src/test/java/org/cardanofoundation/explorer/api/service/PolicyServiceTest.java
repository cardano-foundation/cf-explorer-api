package org.cardanofoundation.explorer.api.service;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.cardanofoundation.explorer.api.mapper.AssetMetadataMapper;
import org.cardanofoundation.explorer.api.mapper.TokenMapper;
import org.cardanofoundation.explorer.api.model.response.token.TokenAddressResponse;
import org.cardanofoundation.explorer.api.model.response.token.TokenFilterResponse;
import org.cardanofoundation.explorer.api.model.response.token.TokenMetadataResponse;
import org.cardanofoundation.explorer.api.projection.AddressTokenProjection;
import org.cardanofoundation.explorer.api.repository.*;
import org.cardanofoundation.explorer.api.service.impl.PolicyServiceImpl;
import org.cardanofoundation.explorer.common.exceptions.BusinessException;
import org.cardanofoundation.explorer.consumercommon.entity.Address;
import org.cardanofoundation.explorer.consumercommon.entity.AssetMetadata;
import org.cardanofoundation.explorer.consumercommon.entity.MultiAsset;
import org.cardanofoundation.explorer.consumercommon.entity.Script;
import org.h2.mvstore.Page;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;

import java.util.List;
import java.util.Optional;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
public class PolicyServiceTest {
    @Mock
    private MultiAssetRepository multiAssetRepository;
    @Mock
    private AssetMetadataRepository assetMetadataRepository;
    @Mock
    private AddressRepository addressRepository;
    @Mock
    private AddressTokenBalanceRepository addressTokenBalanceRepository;
    @Mock
    private AssetMetadataMapper assetMetadataMapper;
    @Mock
    private TokenMapper tokenMapper;
    @Mock
    private ScriptRepository scriptRepository;
    @Mock
    private ObjectMapper objectMapper;
    @InjectMocks
    private PolicyServiceImpl policyService;

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
        when(scriptRepository.findByHash(policyId)).thenReturn(Optional.of(Script.builder().bytes(new byte[0]).build()));
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
        when(scriptRepository.findByHash(policyId)).thenReturn(Optional.of(Script.builder().json("policy").build()));

        var response = policyService.getPolicyDetail(policyId);
        assertEquals(response.getPolicyId(), "1");
        assertEquals(response.getTotalToken(), 1);
        assertEquals(response.getPolicyScript(), "policy");
    }

    @Test
    void testGetPolicyDetail_throwObjectMapper() throws JsonProcessingException {
        String policyId = "1";

        when(multiAssetRepository.countByPolicy(policyId)).thenReturn(1);
        when(scriptRepository.findByHash(policyId)).thenReturn(Optional.of(Script.builder().bytes(new byte[0]).build()));
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

        when(multiAssetRepository.findAllByPolicy(policyId, pageable)).thenReturn(new PageImpl<>(List.of(MultiAsset.builder().policy("1").name("name").build())));
        when(assetMetadataRepository.findBySubjectIn(any())).thenReturn(List.of(AssetMetadata.builder().subject("subject").build()));
        when(tokenMapper.fromMultiAssetToFilterResponse(any())).thenReturn(TokenFilterResponse.builder().policy("1").name("name").build());
        when(assetMetadataMapper.fromAssetMetadata(any())).thenReturn(TokenMetadataResponse.builder().logo("logo").decimals(1).description("description").url("url").build());

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

    @Test
    void testGetHolders_thenReturn() {
        String policyId = "1";
        Pageable pageable = PageRequest.of(0, 10);
        AddressTokenProjection projection = Mockito.mock(AddressTokenProjection.class);
        when(projection.getAddressId()).thenReturn(1L);
        when(projection.getPolicy()).thenReturn("policy");
        when(projection.getTokenName()).thenReturn("token");

        when(addressTokenBalanceRepository.findAddressAndBalanceByMultiAssetIn(policyId, pageable)).thenReturn(new PageImpl<>(List.of(projection)));
        when(addressRepository.findAddressByIdIn(Set.of(1L))).thenReturn(List.of(Address.builder().id(1L).build()));
        when(tokenMapper.fromAddressTokenProjection(any())).thenReturn(TokenAddressResponse.builder().addressId(1L).policy("policy").name("name").build());
        when(assetMetadataRepository.findBySubjectIn(any())).thenReturn(List.of(AssetMetadata.builder().subject("subject").build()));
        when(assetMetadataMapper.fromAssetMetadata(any())).thenReturn(TokenMetadataResponse.builder().url("url").logo("logo").description("description").build());

        var response = policyService.getHolders(policyId, pageable);
        assertEquals(response.getTotalItems(), 1);
        assertEquals(response.getTotalPages(), 1);
        assertEquals(response.getCurrentPage(), 0);
        assertEquals(response.getData().get(0).getName(), "name");
        assertEquals(response.getData().get(0).getPolicy(), "policy");
        assertEquals(response.getData().get(0).getMetadata().getUrl(), "url");
        assertEquals(response.getData().get(0).getMetadata().getLogo(), "logo");
        assertEquals(response.getData().get(0).getMetadata().getDescription(), "description");
    }
}
