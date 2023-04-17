package com.cardano.explorer.service.impl;

import com.cardano.explorer.exception.BusinessCode;
import com.cardano.explorer.mapper.BlockMapper;
import com.cardano.explorer.model.response.BaseFilterResponse;
import com.cardano.explorer.model.response.BlockFilterResponse;
import com.cardano.explorer.model.response.BlockResponse;
import com.cardano.explorer.repository.BlockRepository;
import com.cardano.explorer.repository.SlotLeaderRepository;
import com.cardano.explorer.repository.TxRepository;
import com.cardano.explorer.service.BlockService;
import com.sotatek.cardano.common.entity.BaseEntity;
import com.sotatek.cardano.common.entity.Block;
import com.sotatek.cardano.common.entity.SlotLeader;
import com.sotatek.cardano.common.entity.Tx;
import com.sotatek.cardanocommonapi.exceptions.BusinessException;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.function.Function;
import java.util.stream.Collectors;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@RequiredArgsConstructor
@Log4j2
public class BlockServiceImpl implements BlockService {

  private final BlockRepository blockRepository;
  private final TxRepository txRepository;
  private final SlotLeaderRepository slotLeaderRepository;
  private final BlockMapper blockMapper;

  @Override
  @Transactional(readOnly = true)
  public BlockResponse getBlockDetailByBlockId(String blockId) {
    try {
      Long blockNo = Long.parseLong(blockId);
      Block block = blockRepository.findFirstByBlockNo(blockNo).orElseThrow(
          () -> new BusinessException(BusinessCode.BLOCK_NOT_FOUND)
      );
      return getBlockResponse(block);
    } catch (NumberFormatException e) {
      Block block = blockRepository.findFirstByHash(blockId).orElseThrow(
          () -> new BusinessException(BusinessCode.BLOCK_NOT_FOUND)
      );
      return getBlockResponse(block);
    }

  }
  /**
   * Get block response from entity, calculate totalOutputs and total fees
   * @param block block entity
   * @return block response
   */
  private BlockResponse getBlockResponse(Block block) {
    BlockResponse blockResponse = blockMapper.blockToBlockResponse(block);
    List<Tx> txList = block.getTxList();
    blockResponse.setTotalOutput(
        txList.stream().map(Tx::getOutSum).reduce(BigInteger.ZERO, BigInteger::add));
    blockResponse.setTotalFees(
        txList.stream().map(Tx::getFee).reduce(BigInteger.ZERO, BigInteger::add));
    Integer currentBlockNo = blockRepository.findCurrentBlock().orElseThrow(
        () -> new BusinessException(BusinessCode.BLOCK_NOT_FOUND)
    );
    if(Objects.nonNull(block.getBlockNo())) {
      blockResponse.setConfirmation(currentBlockNo - block.getBlockNo().intValue());
    }
    return blockResponse;
  }

  @Override
  @Transactional(readOnly = true)
  public BaseFilterResponse<BlockFilterResponse> filterBlock(Pageable pageable) {
    Page<Block> blockPage = blockRepository.findAllBlock(pageable);
    return mapperBlockToBlockFilterResponse(blockPage);
  }

  @Override
  @Transactional(readOnly = true)
  public BaseFilterResponse<BlockFilterResponse> getBlockByEpoch(String no, Pageable pageable) {
    try {
      Integer epochNo = Integer.parseInt(no);
      Page<Block> blocks = blockRepository.findBlockByEpochNo(epochNo, pageable);
      return mapperBlockToBlockFilterResponse(blocks);
    } catch (NumberFormatException e) {
      throw new BusinessException(BusinessCode.EPOCH_NOT_FOUND);
    }
  }

  /**
   * Mapping from block entity to block response dto
   *
   * @param blocks list block entity
   * @return list block information in this page
   */
  private BaseFilterResponse<BlockFilterResponse> mapperBlockToBlockFilterResponse(
      Page<Block> blocks) {
    //get slot leader for block
    List<SlotLeader> slotLeaders = slotLeaderRepository.findByIdIn(blocks.getContent().stream().map(
        Block::getSlotLeaderId).collect(Collectors.toList()));
    Map<Long, SlotLeader> slotLeaderMap = slotLeaders.stream().collect(Collectors.toMap(
        BaseEntity::getId, Function.identity()
    ));

    List<Tx> txList = txRepository.findByBlockIn(blocks.toList());

    // create map with key: block_id, value : total output of block
    Map<Long, BigInteger> blockTotalOutputMap = txList.stream().collect(Collectors.groupingBy(
        tx -> tx.getBlock().getId(),
        Collectors.reducing(BigInteger.ZERO, Tx::getOutSum, BigInteger::add)
    ));
    // create map with key: block_id, value : total fee of block
    Map<Long, BigInteger> blockTotalFeeMap = txList.stream().collect(Collectors.groupingBy(
        tx -> tx.getBlock().getId(),
        Collectors.reducing(BigInteger.ZERO, Tx::getFee, BigInteger::add)
    ));

    List<BlockFilterResponse> blockFilterResponseList = new ArrayList<>();

    for (Block block : blocks) {
      block.setSlotLeader(slotLeaderMap.get(block.getSlotLeaderId()));
      BlockFilterResponse blockResponse = blockMapper.blockToBlockFilterResponse(block);
      var totalOutput = blockTotalOutputMap.get(block.getId());
      var totalFees = blockTotalFeeMap.get(block.getId());
      blockResponse.setTotalOutput(Objects.requireNonNullElse(totalOutput, BigInteger.ZERO));
      blockResponse.setTotalFees(Objects.requireNonNullElse(totalFees, BigInteger.ZERO));
      blockFilterResponseList.add(blockResponse);
    }
    return new BaseFilterResponse<>(blocks, blockFilterResponseList);
  }
}
