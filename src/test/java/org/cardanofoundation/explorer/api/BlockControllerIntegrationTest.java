package org.cardanofoundation.explorer.api;

import org.cardanofoundation.explorer.api.config.JacksonMapperDateConfig;
import org.cardanofoundation.explorer.api.config.SpringWebSecurityConfig;
import org.cardanofoundation.explorer.api.config.WebConfig;
import org.cardanofoundation.explorer.api.controller.BlockController;
import org.cardanofoundation.explorer.api.exception.BusinessCode;
import org.cardanofoundation.explorer.api.model.response.BlockResponse;
import org.cardanofoundation.explorer.api.service.BlockService;
import org.cardanofoundation.explorer.api.service.TxService;
import org.cardanofoundation.explorer.api.controller.advice.GlobalRestControllerExceptionHandler;
import org.cardanofoundation.explorer.common.exceptions.BusinessException;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.Import;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.web.context.WebApplicationContext;

import java.util.Objects;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@WebMvcTest(BlockController.class)
@Import({
  SpringWebSecurityConfig.class,
  WebConfig.class,
  JacksonMapperDateConfig.class,
  GlobalRestControllerExceptionHandler.class
})
public class BlockControllerIntegrationTest {

  private static final String NON_EXISTING_BLOCK_ID = "12345";
  private static final String EXISTING_BLOCK_ID = "67890";

  @Autowired private WebApplicationContext context;

  @Autowired private MockMvc mockMvc;

  @MockBean private BlockService blockService;

  @MockBean private TxService txService;

  @BeforeEach
  void setUp() {
    when(blockService.getBlockDetailByBlockId(NON_EXISTING_BLOCK_ID))
        .thenThrow(new BusinessException(BusinessCode.BLOCK_NOT_FOUND));
    when(blockService.getBlockDetailByBlockId(EXISTING_BLOCK_ID)).thenReturn(new BlockResponse());
    /*when(metadataRegistryConfig.networkIsMapped(anyString())).thenReturn(true);
    when(metadataRegistryConfig.sourceFromNetwork(anyString())).thenReturn("mainnet");

    when(v1ApiMetadataIndexer.findSubject(
            "mainnet", "025146866af908340247fe4e9672d5ac7059f1e8534696b5f920c9e66362544843"))
            .thenReturn(Optional.empty());

    final UrlProperty urlProperty = new UrlProperty();
    urlProperty.setSequenceNumber(BigDecimal.valueOf(1));
    urlProperty.setValue("https://fivebinaries.com/nutcoin");
    urlProperty.setSignatures(
            List.of(
                    new AnnotatedSignature(
                            "1ff38761f6d93e58fd48e57c03cbeee848626a430f5d62b6cc555f7969b6636f07dbd0a7bf149cb577e95262c83efceb6bd0ba7724c2b146041d7853c75af603",
                            "08c2ca6654c9e43b41b0b1560ee6a7bb4997629c2646575982934a51ecd71900")));
    when(v1ApiMetadataIndexer.findSubject(
            "mainnet", "025146866af908340247fe4e9672d5ac7059f1e8534696b5f920c9e66362544848"))
            .thenReturn(
                    Optional.of(
                            TokenMetadata.builder()
                                    .subject("025146866af908340247fe4e9672d5ac7059f1e8534696b5f920c9e66362544848")
                                    .url(urlProperty)
                                    .build()));

    when(v1ApiMetadataIndexer.findSubjectSelectProperties(
            "mainnet", "025146866af908340247fe4e9672d5ac7059f1e8534696b5f920c9e66362544848", List.of("url")))
            .thenReturn(
                    Optional.of(
                            TokenMetadata.builder()
                                    .subject("025146866af908340247fe4e9672d5ac7059f1e8534696b5f920c9e66362544848")
                                    .url(urlProperty)
                                    .build()));*/
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
}
