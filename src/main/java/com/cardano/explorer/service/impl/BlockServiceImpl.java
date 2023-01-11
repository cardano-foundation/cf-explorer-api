package com.cardano.explorer.service.impl;

import com.cardano.explorer.exception.BusinessCode;
import com.cardano.explorer.mapper.BlockMapper;
import com.cardano.explorer.model.request.BlockFilterRequest;
import com.cardano.explorer.model.response.BaseFilterResponse;
import com.cardano.explorer.model.response.BlockFilterResponse;
import com.cardano.explorer.model.response.BlockResponse;
import com.cardano.explorer.repository.BlockRepository;
import com.cardano.explorer.repository.SlotLeaderRepository;
import com.cardano.explorer.repository.TxRepository;
import com.cardano.explorer.service.BlockService;
import com.cardano.explorer.specification.BlockSpecification;
import com.sotatek.cardano.common.entity.BaseEntity;
import com.sotatek.cardano.common.entity.Block;
import com.sotatek.cardano.common.entity.SlotLeader;
import com.sotatek.cardano.common.entity.Tx;
import com.sotatek.cardanocommonapi.exceptions.BusinessException;
import java.math.BigDecimal;
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

  private final BlockSpecification blockSpecification;

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
        txList.stream().map(Tx::getOutSum).reduce(BigDecimal.ZERO, BigDecimal::add));
    blockResponse.setTotalFees(
        txList.stream().map(Tx::getFee).reduce(BigDecimal.ZERO, BigDecimal::add));
    return blockResponse;
  }

  @Override
  @Transactional(readOnly = true)
  public BaseFilterResponse<BlockFilterResponse> filterBlock(Pageable pageable,
      BlockFilterRequest request) {
    Page<Block> page;
    if (request != null) {
      page = blockRepository.findAll(blockSpecification.getFilter(request), pageable);
    } else {
      page = blockRepository.findAll(pageable);
    }
    return mapperBlockToBlockFilterResponse(page);
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
    Map<Long, BigDecimal> blockTotalOutputMap = txList.stream().collect(Collectors.groupingBy(
        tx -> tx.getBlock().getId(),
        Collectors.reducing(BigDecimal.ZERO, Tx::getOutSum, BigDecimal::add)
    ));
    // create map with key: block_id, value : total fee of block
    Map<Long, BigDecimal> blockTotalFeeMap = txList.stream().collect(Collectors.groupingBy(
        tx -> tx.getBlock().getId(),
        Collectors.reducing(BigDecimal.ZERO, Tx::getFee, BigDecimal::add)
    ));

    List<BlockFilterResponse> blockFilterResponseList = new ArrayList<>();

    for (Block block : blocks) {
      block.setSlotLeader(slotLeaderMap.get(block.getSlotLeaderId()));
      BlockFilterResponse blockResponse = blockMapper.blockToBlockFilterResponse(block);
      var totalOutput = blockTotalOutputMap.get(block.getId());
      var totalFees = blockTotalFeeMap.get(block.getId());
      blockResponse.setTotalOutput(Objects.requireNonNullElse(totalOutput, BigDecimal.ZERO));
      blockResponse.setTotalFees(Objects.requireNonNullElse(totalFees, BigDecimal.ZERO));
      blockFilterResponseList.add(blockResponse);
    }
    return new BaseFilterResponse<>(blocks, blockFilterResponseList);
  }
}
