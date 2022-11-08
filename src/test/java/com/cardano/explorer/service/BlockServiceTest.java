package com.cardano.explorer.service;

import static org.assertj.core.api.AssertionsForClassTypes.assertThat;
import static org.assertj.core.api.AssertionsForClassTypes.assertThatThrownBy;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.Mockito.when;

import com.cardano.explorer.entity.Block;
import com.cardano.explorer.entity.Tx;
import com.cardano.explorer.exception.BusinessException;
import com.cardano.explorer.mapper.BlockMapper;
import com.cardano.explorer.model.BlockResponse;
import com.cardano.explorer.repository.BlockRepository;
import com.cardano.explorer.repository.TxRepository;
import com.cardano.explorer.service.impl.BlockServiceImpl;
import java.math.BigDecimal;
import java.sql.Timestamp;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.test.context.junit.jupiter.SpringExtension;

@ExtendWith(SpringExtension.class)
@SpringBootTest
class BlockServiceTest {

  @InjectMocks
  private BlockServiceImpl blockService;

  @Mock
  private BlockRepository blockRepository;

  @Mock
  private BlockMapper blockMapperMock;

  @Mock
  private TxRepository txRepository;

  @Autowired
  private BlockMapper blockMapper;

  private Block block;

  private List<Block> blockPage;

  private List<Tx> txList;

  @BeforeEach
  public void setup() {
    block = Block.builder()
        .blockNo(3917658)
        .id(7626461L)
        .epochNo(239)
        .slotNo(73070327L)
        .epochSlotNo(191927)
        .size(7085)
        .time(Timestamp.valueOf("2022-11-03 01:39:03.000"))
        .txCount(2L)
        .build();
    blockPage = Collections.singletonList(block);
    Tx tx = Tx.builder()
        .id(10331018L)
        .block(block)
        .blockIndex(0L)
        .outSum(new BigDecimal("25936214477"))
        .fee(new BigDecimal("172365"))
        .size(386)
        .validContract(true)
        .build();
    Tx tx2 = Tx.builder()
        .id(10331019L)
        .block(block)
        .blockIndex(1L)
        .outSum(new BigDecimal("35453513"))
        .fee(new BigDecimal("469127"))
        .size(6697)
        .validContract(true)
        .build();
    txList = Arrays.asList(tx, tx2);
  }

  @Test
  void whenGetBlockWithValidBlockNo_shouldReturnBlock() {
    when(blockRepository.findByBlockNo(anyInt())).thenReturn(Optional.ofNullable(block));
    when(blockMapperMock.blockToBlockResponse(any(Block.class)))
        .thenReturn(blockMapper.blockToBlockResponse(block));
    BlockResponse response = blockService.getBlockDetail(3917658);
    assertThat(response.getBlockNo()).isNotNull();
    assertThat(response.getBlockNo()).isEqualTo(3917658);
  }

  @Test
  void whenGetBlockWithInvalidBlockNo_shouldThrow() {
    when(blockRepository.findByBlockNo(-1)).thenReturn(Optional.empty());
    assertThatThrownBy(() -> blockService.getBlockDetail(-1)).isInstanceOf(BusinessException.class);
  }

  @Test
  void whenGetBlockWithPage_shouldReturnBlockList() {
    when(blockRepository.findAllBlock(PageRequest.of(0, 20))).thenReturn(blockPage);
    when(blockMapperMock.blockToBlockFilterResponse(any(Block.class)))
        .thenReturn(blockMapper.blockToBlockFilterResponse(block));
    when(blockRepository.countAll()).thenReturn(Optional.of(1));
    when(txRepository.findByBlockIn(any())).thenReturn(txList);
    var response =
        blockService.getAllBlock(Pageable.ofSize(20).withPage(0));
    assertThat(response.getData().size()).isSameAs(1);
  }

  @Test
  void whenGetBlockByValidEpochNo_shouldReturnBlockList() {
    when(blockRepository.findByEpochNo(200, PageRequest.of(0, 20))).thenReturn(new PageImpl<>(blockPage, PageRequest.of(0, 20), 1));
    when(blockMapperMock.blockToBlockFilterResponse(any(Block.class)))
        .thenReturn(blockMapper.blockToBlockFilterResponse(block));
    when(txRepository.findByBlockIn(any())).thenReturn(txList);
    var response = blockService.getBlockByEpochNo(200,
        Pageable.ofSize(20).withPage(0));
    assertThat(response.getData().size()).isSameAs(1);
  }

  @Test
  void whenGetBlockByInvalidEpochNo_shouldReturnEmpty() {
    when(blockRepository.findByEpochNo(-1, PageRequest.of(0, 20))).thenReturn(Page.empty());
    when(blockMapperMock.blockToBlockFilterResponse(any(Block.class)))
        .thenReturn(blockMapper.blockToBlockFilterResponse(block));
    when(txRepository.findByBlockIn(any())).thenReturn(txList);
    var response = blockService.getBlockByEpochNo(-1,
        Pageable.ofSize(20).withPage(0));
    assertThat(response.getData().size()).isSameAs(0);
  }

}
