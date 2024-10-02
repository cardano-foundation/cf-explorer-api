package org.cardanofoundation.explorer.api.service;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.math.BigInteger;
import java.util.*;

import org.springframework.data.domain.*;

import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import org.cardanofoundation.explorer.api.exception.NoContentException;
import org.cardanofoundation.explorer.api.mapper.BlockMapper;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.BlockFilterResponse;
import org.cardanofoundation.explorer.api.model.response.BlockResponse;
import org.cardanofoundation.explorer.api.projection.PoolMintBlockProjection;
import org.cardanofoundation.explorer.api.repository.ledgersync.BlockRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.SlotLeaderRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.TxRepository;
import org.cardanofoundation.explorer.api.service.impl.BlockServiceImpl;
import org.cardanofoundation.explorer.common.entity.ledgersync.Block;
import org.cardanofoundation.explorer.common.entity.ledgersync.Tx;
import org.cardanofoundation.explorer.common.exception.BusinessException;

@ExtendWith(MockitoExtension.class)
public class BlockServiceTest {
  @InjectMocks private BlockServiceImpl blockService;

  @Mock private BlockRepository blockRepository;

  @Mock private TxRepository txRepository;

  @Mock private SlotLeaderRepository slotLeaderRepository;

  @Mock private BlockMapper blockMapper;

  @Test
  public void testGetBlockDetailByBlockId_WhenBlockNoExists() {
    // Mock input and expected output
    String blockId = "123";
    Block block = new Block();
    block.setBlockNo(123L);
    BlockResponse expectedResponse = new BlockResponse();

    PoolMintBlockProjection projection = Mockito.mock(PoolMintBlockProjection.class);

    // Mock repository method
    when(blockRepository.findFirstByBlockNo(123L)).thenReturn(Optional.of(block));
    when(blockMapper.blockToBlockResponse(block)).thenReturn(expectedResponse);
    when(txRepository.findAllByBlock(block)).thenReturn(new ArrayList<>());
    when(blockRepository.findCurrentBlock()).thenReturn(Optional.of(0));
    when(blockRepository.getPoolInfoThatMintedBlock(any())).thenReturn(projection);

    // Call the service method
    BlockResponse response = blockService.getBlockDetailByBlockId(blockId);

    // Verify the repository method was called and the response is correct
    verify(blockRepository).findFirstByBlockNo(123L);
    assertEquals(expectedResponse, response);
  }

  @Test
  public void testGetBlockDetailByBlockId_WhenBlockNoNotFound() {
    // Mock input
    String blockId = "123";

    // Mock repository method
    when(blockRepository.findFirstByBlockNo(123L)).thenReturn(Optional.empty());

    // Call the service method and expect BusinessException to be thrown
    assertThrows(BusinessException.class, () -> blockService.getBlockDetailByBlockId(blockId));
  }

  @Test
  public void testGetBlockDetailByBlockId_WhenBlockHashExists() {
    // Mock input and expected output
    String blockId = "hash123";
    Block block = new Block();
    BlockResponse expectedResponse = new BlockResponse();

    PoolMintBlockProjection projection = Mockito.mock(PoolMintBlockProjection.class);
    when(projection.getPoolView()).thenReturn("poolView");
    when(projection.getPoolTicker()).thenReturn("poolTicker");
    when(projection.getPoolName()).thenReturn("poolName");

    // Mock repository method
    when(blockRepository.findFirstByHash("hash123")).thenReturn(Optional.of(block));
    when(blockMapper.blockToBlockResponse(block)).thenReturn(expectedResponse);
    when(txRepository.findAllByBlock(block)).thenReturn(new ArrayList<>());
    when(blockRepository.findCurrentBlock()).thenReturn(Optional.of(0));
    when(blockRepository.getPoolInfoThatMintedBlock(any())).thenReturn(projection);

    // Call the service method
    BlockResponse response = blockService.getBlockDetailByBlockId(blockId);

    // Verify the repository method was called and the response is correct
    verify(blockRepository).findFirstByHash("hash123");
    assertEquals(expectedResponse, response);
    assertEquals(response.getPoolView(), "poolView");
  }

  @Test
  public void testGetBlockDetailByBlockId_WhenBlockHashNotFound() {
    // Mock input
    String blockId = "hash123";

    // Mock repository method
    when(blockRepository.findFirstByHash("hash123")).thenReturn(Optional.empty());

    // Call the service method and expect BusinessException to be thrown
    assertThrows(BusinessException.class, () -> blockService.getBlockDetailByBlockId(blockId));
  }

  @Test
  public void testGetBlockDetailByBlockId_ThrowCurrentBlock() {
    // Mock input
    String blockId = "hash123";
    Block block = new Block();
    BlockResponse expectedResponse = new BlockResponse();
    PoolMintBlockProjection projection = Mockito.mock(PoolMintBlockProjection.class);

    // Mock repository method
    when(blockRepository.findFirstByHash("hash123")).thenReturn(Optional.of(block));
    when(blockMapper.blockToBlockResponse(block)).thenReturn(expectedResponse);
    when(txRepository.findAllByBlock(block)).thenReturn(new ArrayList<>());
    when(blockRepository.findCurrentBlock()).thenReturn(Optional.empty());
    when(blockRepository.getPoolInfoThatMintedBlock(any())).thenReturn(projection);

    // Verify the repository method was called and the response is correct
    assertThrows(BusinessException.class, () -> blockService.getBlockDetailByBlockId(blockId));
  }

  @Test
  public void testFilterBlock() {
    // Mock input and expected output
    Pageable pageable = PageRequest.of(0, 10, Sort.by("txCount").descending());
    Page<Block> blockPage = new PageImpl<>(Collections.singletonList(new Block()));
    List<Tx> txList =
        List.of(
            Tx.builder()
                .block(Block.builder().id(1L).build())
                .id(1L)
                .outSum(BigInteger.ONE)
                .fee(BigInteger.ONE)
                .build(),
            Tx.builder()
                .block(Block.builder().id(2L).build())
                .id(2L)
                .outSum(BigInteger.TWO)
                .fee(BigInteger.TWO)
                .build());

    // Mock repository method
    when(blockRepository.findAllBlock(any())).thenReturn(blockPage);
    when(blockMapper.blockToBlockFilterResponse(any(Block.class)))
        .thenReturn(new BlockFilterResponse());
    when(slotLeaderRepository.findByIdIn(anyList())).thenReturn(Collections.emptyList());

    // Call the service method
    BaseFilterResponse<BlockFilterResponse> response = blockService.filterBlock(pageable);

    // Verify the repository methods were called and the response is correct
    verify(blockRepository).findAllBlock(pageable);
    verify(blockMapper).blockToBlockFilterResponse(any(Block.class));
    verify(slotLeaderRepository).findByIdIn(anyList());
    assertEquals(response.getData().size(), 1);
    assertEquals(response.getTotalItems(), 1);
  }

  @Test
  public void testGetBlockByEpoch_WhenEpochNoExists() {
    // Mock input and expected output
    String no = "1";
    Integer epochNo = 1;
    Pageable pageable = PageRequest.of(0, 10);
    Page<Block> blocks = new PageImpl<>(Collections.singletonList(new Block()));

    // Mock repository method
    when(blockRepository.findBlockByEpochNo(epochNo, pageable)).thenReturn(blocks);
    when(blockMapper.blockToBlockFilterResponse(any(Block.class)))
        .thenReturn(new BlockFilterResponse());
    when(slotLeaderRepository.findByIdIn(anyList())).thenReturn(Collections.emptyList());

    // Call the service method
    BaseFilterResponse<BlockFilterResponse> response = blockService.getBlockByEpoch(no, pageable);

    // Verify the repository methods were called and the response is correct
    verify(blockRepository).findBlockByEpochNo(epochNo, pageable);
    verify(blockMapper).blockToBlockFilterResponse(any(Block.class));
    verify(slotLeaderRepository).findByIdIn(anyList());
    assertEquals(response.getData().size(), 1);
    assertEquals(response.getTotalItems(), 1);
  }

  @Test
  public void testGetBlockByEpoch_WhenEpochNoNotFound() {
    // Mock input
    String no = "invalid";

    // Call the service method and expect BusinessException to be thrown
    assertThrows(
        NoContentException.class, () -> blockService.getBlockByEpoch(no, Pageable.unpaged()));
  }
}
