package org.cardanofoundation.explorer.api.controller;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.util.List;
import java.util.Map;
import java.util.Set;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.Import;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;

import com.fasterxml.jackson.databind.ObjectMapper;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import org.cardanofoundation.explorer.api.common.enumeration.TxPurposeType;
import org.cardanofoundation.explorer.api.config.JacksonMapperDateConfig;
import org.cardanofoundation.explorer.api.config.SpringWebSecurityConfig;
import org.cardanofoundation.explorer.api.config.WebConfig;
import org.cardanofoundation.explorer.api.controller.advice.GlobalRestControllerExceptionHandler;
import org.cardanofoundation.explorer.api.interceptor.AuthInterceptor;
import org.cardanofoundation.explorer.api.interceptor.auth.RoleFilterMapper;
import org.cardanofoundation.explorer.api.model.request.script.smartcontract.SmartContractFilterRequest;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.script.nativescript.NativeScriptFilterResponse;
import org.cardanofoundation.explorer.api.model.response.script.nativescript.NativeScriptResponse;
import org.cardanofoundation.explorer.api.model.response.script.smartcontract.SmartContractDetailResponse;
import org.cardanofoundation.explorer.api.model.response.script.smartcontract.SmartContractFilterResponse;
import org.cardanofoundation.explorer.api.model.response.script.smartcontract.SmartContractTxResponse;
import org.cardanofoundation.explorer.api.model.response.search.ScriptSearchResponse;
import org.cardanofoundation.explorer.api.model.response.token.TokenAddressResponse;
import org.cardanofoundation.explorer.api.model.response.token.TokenFilterResponse;
import org.cardanofoundation.explorer.api.service.ScriptService;
import org.cardanofoundation.explorer.common.entity.enumeration.ScriptType;

@WebMvcTest(ScriptController.class)
@Import({
  SpringWebSecurityConfig.class,
  WebConfig.class,
  JacksonMapperDateConfig.class,
  GlobalRestControllerExceptionHandler.class,
  RoleFilterMapper.class
})
@AutoConfigureMockMvc(addFilters = false)
public class ScriptControllerTest {

  @Autowired private MockMvc mockMvc;

  @MockBean private AuthInterceptor authInterceptor;

  @MockBean private ScriptService scriptService;

  @BeforeEach
  void preControllerTest() throws Exception {
    when(authInterceptor.preHandle(any(), any(), any())).thenReturn(true);
  }

  @Test
  void testGetSmartContracts_thenReturn() throws Exception {
    SmartContractFilterResponse smartContractFilterResponse =
        SmartContractFilterResponse.builder()
            .scriptHash("hash")
            .scriptVersion(ScriptType.PLUTUSV3)
            .txPurposes(Set.of(TxPurposeType.MINT))
            .txCount(1L)
            .build();
    BaseFilterResponse<SmartContractFilterResponse> response =
        new BaseFilterResponse<>(List.of(smartContractFilterResponse), 1);

    when(scriptService.getSmartContracts(
            any(SmartContractFilterRequest.class), any(Pageable.class)))
        .thenReturn(response);

    mockMvc
        .perform(
            get("/api/v1/scripts/contracts")
                .param("scriptVersion", "PLUTUSV1")
                .param("txPurpose", "MINT"))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$").exists())
        .andExpect(jsonPath("$.data[0].scriptHash").value("hash"))
        .andExpect(jsonPath("$.data[0].scriptVersion").value("PLUTUSV3"))
        .andExpect(jsonPath("$.data[0].txPurposes[0]").value("MINT"))
        .andExpect(jsonPath("$.data[0].txCount").value("1"));
  }

  @Test
  void testNativeScripts_thenReturn() throws Exception {
    NativeScriptFilterResponse nativeScriptFilterResponse =
        NativeScriptFilterResponse.builder()
            .scriptHash("hash")
            .numberOfTokens(1L)
            .numberOfAssetHolders(1L)
            .build();
    BaseFilterResponse<NativeScriptFilterResponse> response =
        new BaseFilterResponse<>(List.of(nativeScriptFilterResponse), 1);

    when(scriptService.getNativeScripts(any(), any(Pageable.class))).thenReturn(response);

    mockMvc
        .perform(get("/api/v1/scripts/native-scripts"))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$").exists())
        .andExpect(jsonPath("$.data[0].numberOfTokens").value("1"))
        .andExpect(jsonPath("$.data[0].numberOfAssetHolders").value("1"))
        .andExpect(jsonPath("$.data[0].scriptHash").value("hash"));
  }

    @Test
    void testGetNativeScriptDetail() throws Exception {
      String scriptHash = "a5bb0e5bb275a573d744a021f9b3bff73595468e002755b447e01559";
      NativeScriptResponse response =
          NativeScriptResponse.builder()
              .script("script")
              .numberOfTokens(1L)
              .scriptHash(scriptHash)
              .build();

      when(scriptService.getNativeScriptDetail(scriptHash)).thenReturn(response);

      mockMvc
          .perform(get("/api/v1/scripts/native-scripts/{scriptHash}", scriptHash))
          .andExpect(status().isOk())
          .andExpect(jsonPath("$").exists())
          .andExpect(jsonPath("$.script").value("script"))
          .andExpect(jsonPath("$.numberOfTokens").value("1"))
          .andExpect(jsonPath("$.scriptHash").value(scriptHash));

      verify(scriptService).getNativeScriptDetail(scriptHash);
    }

  @Test
  void testVerifyContract() throws Exception {
    String scriptHash = "a5bb0e5bb275a573d744a021f9b3bff73595468e002755b447e01559";

    when(scriptService.verifyNativeScript(anyString(), anyString())).thenReturn("true");

    mockMvc
        .perform(
            post("/api/v1/scripts/native-scripts/{scriptHash}/verify", scriptHash)
                .contentType(MediaType.APPLICATION_JSON)
                .content(asJsonString(Map.of("type", "all"))))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$").value(true));

    verify(scriptService).verifyNativeScript(anyString(), anyString());
  }

  @Test
  void testGetToken() throws Exception {
    String scriptHash = "3a9241cd79895e3a8d65261b40077d4437ce71e9d7c8c6c00e3f658e";
    TokenFilterResponse response =
        TokenFilterResponse.builder().id(1L).fingerprint("fingerprint").build();

    when(scriptService.getNativeScriptTokens(
            scriptHash, PageRequest.of(0, 1, Sort.by("txCount").descending())))
        .thenReturn(new BaseFilterResponse<>(List.of(response), 1));

    mockMvc
        .perform(
            get("/api/v1/scripts/native-scripts/{scriptHash}/tokens", scriptHash)
                .param("page", "0")
                .param("size", "1"))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.data[0].fingerprint").value("fingerprint"))
        .andExpect(jsonPath("$.data[0].id").value("1"));

    verify(scriptService)
        .getNativeScriptTokens(scriptHash, PageRequest.of(0, 1, Sort.by("txCount").descending()));
  }

    @Test
    void testGetHolder() throws Exception {
      String scriptHash = "3a9241cd79895e3a8d65261b40077d4437ce71e9d7c8c6c00e3f658e";
      TokenAddressResponse response =
          TokenAddressResponse.builder()
              .address("addreses")
              .name("name")
              .fingerprint("fingerprint")
              .build();

      when(scriptService.getNativeScriptHolders(scriptHash, PageRequest.of(0, 1)))
          .thenReturn(new BaseFilterResponse<>(List.of(response), 1));

      mockMvc
          .perform(
              get("/api/v1/scripts/native-scripts/{scriptHash}/holders", scriptHash)
                  .param("page", "0")
                  .param("size", "1"))
          .andExpect(status().isOk())
          .andExpect(jsonPath("$.data[0].name").value("name"))
          .andExpect(jsonPath("$.data[0].fingerprint").value("fingerprint"));

      verify(scriptService).getNativeScriptHolders(scriptHash, PageRequest.of(0, 1));
    }

  @Test
  void testGetSmartContracts() throws Exception {
    SmartContractFilterResponse response =
        SmartContractFilterResponse.builder().scriptHash("scriptHash").txCount(1L).build();

    when(scriptService.getSmartContracts(
            any(SmartContractFilterRequest.class), any(Pageable.class)))
        .thenReturn(new BaseFilterResponse<>(List.of(response), 1));

    mockMvc
        .perform(
            get("/api/v1/scripts/contracts")
                .accept(MediaType.APPLICATION_JSON)
                .param("page", "0")
                .param("size", "1"))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.data[0].scriptHash").value("scriptHash"));

    verify(scriptService)
        .getSmartContracts(any(SmartContractFilterRequest.class), any(Pageable.class));
  }

    @Test
    void testGetSmartContractsDetails() throws Exception {
      String scriptHash = "3a9241cd79895e3a8d65261b40077d4437ce71e9d7c8c6c00e3f658e";
      SmartContractDetailResponse response =
          SmartContractDetailResponse.builder()
              .scriptHash(scriptHash)
              .scriptType(ScriptType.TIMELOCK)
              .build();

      when(scriptService.getSmartContractDetail(scriptHash)).thenReturn(response);

      mockMvc
          .perform(get("/api/v1/scripts/contracts/{scriptHash}", scriptHash))
          .andExpect(status().isOk())
          .andExpect(jsonPath("$.scriptHash").value(scriptHash))
          .andExpect(jsonPath("$.scriptType").value(String.valueOf(ScriptType.TIMELOCK)));

      verify(scriptService).getSmartContractDetail(scriptHash);
    }

  @Test
  void testGetSmartContractsTxs() throws Exception {
    String scriptHash = "3a9241cd79895e3a8d65261b40077d4437ce71e9d7c8c6c00e3f658e";
    SmartContractTxResponse response =
        SmartContractTxResponse.builder().hash(scriptHash).epochNo(1).build();

    when(scriptService.getSmartContractTxs(anyString(), any(Pageable.class)))
        .thenReturn(new BaseFilterResponse<>(List.of(response), 1));

    mockMvc
        .perform(
            get("/api/v1/scripts/contracts/{scriptHash}/txs", scriptHash)
                .param("page", "0")
                .param("size", "1"))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.data[0].hash").value(scriptHash))
        .andExpect(jsonPath("$.data[0].epochNo").value("1"));

    verify(scriptService).getSmartContractTxs(anyString(), any(Pageable.class));
  }

  @Test
  void testGetSmartContractExecutionDetail() throws Exception {
    String scriptHash = "hash";
    String txHash = "txHash";
    String response = "res";

    when(scriptService.getContractExecutions(anyString(), anyString()))
        .thenReturn(Set.of(response));

    mockMvc
        .perform(
            get("/api/v1/scripts/contract-executions/{txHash}/{scriptHash}", txHash, scriptHash))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$").exists());

    verify(scriptService).getContractExecutions(anyString(), anyString());
  }

  @Test
  void testGetScriptByHash() throws Exception {
    String scriptHash = "hash";

    ScriptSearchResponse response =
        ScriptSearchResponse.builder().scriptHash(scriptHash).isNativeScript(false).build();

    when(scriptService.searchScript(scriptHash)).thenReturn(response);

    mockMvc
        .perform(get("/api/v1/scripts/search/{scriptHash}", scriptHash))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.nativeScript").value(false))
        .andExpect(jsonPath("$.scriptHash").value(scriptHash));

    verify(scriptService).searchScript(scriptHash);
  }

  public static String asJsonString(final Object obj) {
    try {
      return new ObjectMapper().writeValueAsString(obj);
    } catch (Exception e) {
      throw new RuntimeException(e);
    }
  }
}
