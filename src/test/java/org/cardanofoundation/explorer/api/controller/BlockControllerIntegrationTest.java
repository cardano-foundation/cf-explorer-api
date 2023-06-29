package org.cardanofoundation.explorer.api.controller;

import org.cardanofoundation.explorer.api.config.JacksonMapperDateConfig;
import org.cardanofoundation.explorer.api.config.SpringWebSecurityConfig;
import org.cardanofoundation.explorer.api.config.WebConfig;
import org.cardanofoundation.explorer.api.controller.advice.GlobalRestControllerExceptionHandler;
import org.cardanofoundation.explorer.api.exception.BusinessCode;
import org.cardanofoundation.explorer.api.interceptor.AuthInterceptor;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.BlockFilterResponse;
import org.cardanofoundation.explorer.api.model.response.BlockResponse;
import org.cardanofoundation.explorer.api.model.response.TxFilterResponse;
import org.cardanofoundation.explorer.api.service.BlockService;
import org.cardanofoundation.explorer.api.service.TxService;
import org.cardanofoundation.explorer.common.exceptions.BusinessException;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.context.annotation.Import;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.web.context.WebApplicationContext;

import java.util.Arrays;
import java.util.List;
import java.util.Objects;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;


@WebMvcTest(BlockController.class)
@Import({
  SpringWebSecurityConfig.class,
  WebConfig.class,
  JacksonMapperDateConfig.class,
  GlobalRestControllerExceptionHandler.class
})
@AutoConfigureMockMvc(addFilters = false)
public class BlockControllerIntegrationTest {

  private static final String NON_EXISTING_BLOCK_ID = "12345";
  private static final String EXISTING_BLOCK_ID = "67890";

  @Autowired private WebApplicationContext context;

  @Autowired private MockMvc mockMvc;

  @MockBean private BlockService blockService;

  @MockBean private TxService txService;

  @MockBean
  private AuthInterceptor authInterceptor;

  @BeforeEach
  void preControllerTest() throws Exception {
    when(authInterceptor.preHandle(any(), any(), any())).thenReturn(true);
  }

  @BeforeEach
  void setUp() {
    when(blockService.getBlockDetailByBlockId(NON_EXISTING_BLOCK_ID))
        .thenThrow(new BusinessException(BusinessCode.BLOCK_NOT_FOUND));
    when(blockService.getBlockDetailByBlockId(EXISTING_BLOCK_ID)).thenReturn(new BlockResponse());
  }

  @Test
  public void blockQueryShouldReturnNotFoundOnNonExistingBlock() throws Exception {
    mockMvc
        .perform(get("/api/v1/blocks/{blockId}", NON_EXISTING_BLOCK_ID))
        .andExpect(result -> assertTrue(result.getResolvedException() instanceof BusinessException))
        .andExpect(
            result ->
                assertEquals(
                    ((BusinessException) Objects.requireNonNull(result.getResolvedException()))
                        .getErrorCode(),
                    BusinessCode.BLOCK_NOT_FOUND.getServiceErrorCode()))
        .andExpect(status().isBadRequest());
  }

  @Test
  public void blockQueryShouldReturnBlockIfBlockExists() throws Exception {
    mockMvc.perform(get("/api/v1/blocks/" + EXISTING_BLOCK_ID)).andExpect(status().isOk());
  }

  @Test
  public void testGetBlockDetailByBlockId() throws Exception {
    // Mock request and response objects
    String blockId = "testBlockId";
    BlockResponse mockResponse = BlockResponse.builder()
            .hash(blockId)
            .build();

    // Mock the service method
    when(blockService.getBlockDetailByBlockId(blockId)).thenReturn(mockResponse);

    // Perform the GET request
    mockMvc.perform(get("/api/v1/blocks/{blockId}", blockId))
            .andExpect(status().isOk())
            .andExpect(jsonPath("$.hash").exists());

    // Verify that the service method was called with the correct argument
    verify(blockService).getBlockDetailByBlockId(blockId);
  }

  @Test
  public void testGetAll() throws Exception {
    // Mock request and response objects
    Pageable pageable = PageRequest.of(0, 20, Sort.Direction.DESC, "id");
    List<BlockFilterResponse> mockResponse = Arrays.asList(
            new BlockFilterResponse(),
            new BlockFilterResponse()
    );
    BaseFilterResponse<BlockFilterResponse> response = new BaseFilterResponse<>(mockResponse, pageable.getPageSize());

    // Mock the service method
    when(blockService.filterBlock(any())).thenReturn(response);

    // Perform the GET request
    mockMvc.perform(get("/api/v1/blocks"))
            .andExpect(status().isOk())
            .andExpect(jsonPath("$.data").isArray())
            .andExpect(jsonPath("$.data.length()").value(mockResponse.size()));

    // Verify that the service method was called with the correct argument
    verify(blockService).filterBlock(pageable);
  }

  @Test
  public void testGetTransactionsByBlock() throws Exception {
    // Mock request and response objects
    String blockId = "testBlockId";
    Pageable pageable = PageRequest.of(0, 20, Sort.Direction.DESC, "blockId", "blockIndex");
    List<TxFilterResponse> mockResponse = Arrays.asList(
            new TxFilterResponse(),
            new TxFilterResponse()
    );
    BaseFilterResponse<TxFilterResponse> response = new BaseFilterResponse<>(mockResponse, pageable.getPageSize());

    // Mock the service method
    when(txService.getTransactionsByBlock(any(), any())).thenReturn(response);

    // Perform the GET request
    mockMvc.perform(get("/api/v1/blocks/{blockId}/txs", blockId))
            .andExpect(status().isOk())
            .andExpect(jsonPath("$.data").isArray())
            .andExpect(jsonPath("$.data.length()").value(mockResponse.size()));

    // Verify that the service method was called with the correct arguments
    verify(txService).getTransactionsByBlock(blockId, pageable);
  }
}
