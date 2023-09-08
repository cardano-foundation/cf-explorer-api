package org.cardanofoundation.explorer.api.service.impl;

import java.math.BigInteger;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.function.Function;
import java.util.stream.Collectors;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.cardanofoundation.explorer.api.exception.BusinessCode;
import org.cardanofoundation.explorer.api.mapper.BlockMapper;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.BlockFilterResponse;
import org.cardanofoundation.explorer.api.model.response.BlockResponse;
import org.cardanofoundation.explorer.api.repository.BlockRepository;
import org.cardanofoundation.explorer.api.repository.CustomBlockRepository;
import org.cardanofoundation.explorer.api.repository.SlotLeaderRepository;
import org.cardanofoundation.explorer.api.repository.TxRepository;
import org.cardanofoundation.explorer.api.service.BlockService;
import org.cardanofoundation.explorer.common.exceptions.BusinessException;
import org.cardanofoundation.explorer.common.exceptions.NoContentException;
import org.cardanofoundation.explorer.consumercommon.entity.BaseEntity;
import org.cardanofoundation.explorer.consumercommon.entity.Block;
import org.cardanofoundation.explorer.consumercommon.entity.Block_;
import org.cardanofoundation.explorer.consumercommon.entity.SlotLeader;
import org.cardanofoundation.explorer.consumercommon.entity.Tx;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort.Direction;
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
  private final CustomBlockRepository customBlockRepository;

  public static final String MIN_TIME = "1970-01-01 00:00:00";
  public static final String MAX_TIME = "9999-01-01 00:00:00";

  @Override
  @Transactional(readOnly = true)
  public BlockResponse getBlockDetailByBlockId(String blockId) {
    try {
      Long blockNo = Long.parseLong(blockId);
      Block block =
          blockRepository
              .findFirstByBlockNo(blockNo)
              .orElseThrow(() -> new BusinessException(BusinessCode.BLOCK_NOT_FOUND));
      return getBlockResponse(block);
    } catch (NumberFormatException e) {
      Block block =
          blockRepository
              .findFirstByHash(blockId)
              .orElseThrow(() -> new BusinessException(BusinessCode.BLOCK_NOT_FOUND));
      return getBlockResponse(block);
    }
  }

  /**
   * Get block response from entity, calculate totalOutputs and total fees
   *
   * @param block block entity
   * @return block response
   */
  private BlockResponse getBlockResponse(Block block) {
    BlockResponse blockResponse = blockMapper.blockToBlockResponse(block);
    List<Tx> txList = txRepository.findAllByBlock(block);
    blockResponse.setTotalOutput(
        txList.stream().map(Tx::getOutSum).reduce(BigInteger.ZERO, BigInteger::add));
    blockResponse.setTotalFees(
        txList.stream().map(Tx::getFee).reduce(BigInteger.ZERO, BigInteger::add));
    Integer currentBlockNo =
        blockRepository
            .findCurrentBlock()
            .orElseThrow(() -> new BusinessException(BusinessCode.BLOCK_NOT_FOUND));
    if (Objects.nonNull(block.getBlockNo())) {
      blockResponse.setConfirmation(currentBlockNo - block.getBlockNo().intValue());
    }
    return blockResponse;
  }

  @Override
  @Transactional(readOnly = true)
  public BaseFilterResponse<BlockFilterResponse> filterBlock(Pageable pageable) {
    long totalElements = blockRepository.countAllBlock();
    Direction direction = null;
    String sortField = null;
    if (pageable.getSort().isSorted()) {
      direction = pageable.getSort().stream().findFirst().get().getDirection();
      sortField = pageable.getSort().stream().findFirst().get().getProperty();
    }
    Page<Block> blockPage;
    if (Direction.ASC.equals(direction) && Block_.TIME.equals(sortField)) {
      blockPage = getBlockFilterListByTimeAsc(pageable, totalElements);
    } else if ((Direction.DESC.equals(direction) && Block_.TIME.equals(sortField))
        || (Objects.isNull(direction) || Objects.isNull(sortField))) {
      blockPage = getBlockFilterListByTimeDesc(pageable, totalElements);
    } else {
      blockPage = blockRepository.findAllBlock(pageable);
    }
    return mapperBlockToBlockFilterResponse(blockPage);
  }

  private Page<Block> getBlockFilterListByTimeAsc(Pageable pageable, long totalElements) {
    long firstBlockIndexOfPage = (long) pageable.getPageNumber() * pageable.getPageSize();
    long currentMaxBlockNo = blockRepository.findCurrentBlock().map(Long::valueOf).orElse(0L);

    Timestamp firstBlockTimeOfPage =
        blockRepository
            .findFirstByBlockNo(Math.min(firstBlockIndexOfPage, currentMaxBlockNo))
            .map(Block::getTime)
            .orElse(Timestamp.valueOf(MIN_TIME));

    List<Block> ebbBlocks = blockRepository.findGenesisAndEbbBlockInfo();
    int ebbBlockCountBeforeFirstBlockOfPage =
        (int)
            ebbBlocks.stream()
                .filter(ebbBlock -> ebbBlock.getTime().before(firstBlockTimeOfPage))
                .count();

    long actualFirstBlockIndexOfPage;
    if (ebbBlockCountBeforeFirstBlockOfPage > 0) {
      actualFirstBlockIndexOfPage = firstBlockIndexOfPage + 1 - ebbBlockCountBeforeFirstBlockOfPage;
    } else {
      actualFirstBlockIndexOfPage = firstBlockIndexOfPage;
    }

    List<Block> blockList =
        customBlockRepository.findByBlockNoAndSpecifiedOffset(
            actualFirstBlockIndexOfPage, pageable.getPageSize(), Direction.ASC);

    if (blockList.isEmpty()) {
      return new PageImpl<>(Collections.emptyList(), pageable, totalElements);
    }

    Block firstBlockOfPage = blockList.get(0);
    Block lastBlockOfPage = blockList.get(blockList.size() - 1);

    List<Block> ebbBlockProjectionsInPage =
        ebbBlocks.stream()
            .filter(
                ebbBlock -> {
                  Timestamp blockTime = ebbBlock.getTime();
                  return blockTime.compareTo(firstBlockOfPage.getTime()) >= 0
                      && blockTime.compareTo(lastBlockOfPage.getTime()) <= 0;
                })
            .toList();

    blockList.addAll(ebbBlockProjectionsInPage);
    blockList.sort(Comparator.comparingLong(Block::getId));
    if (blockList.size() > pageable.getPageSize()) {
      blockList = blockList.subList(0, pageable.getPageSize());
    }
    return new PageImpl<>(blockList, pageable, totalElements);
  }

  private Page<Block> getBlockFilterListByTimeDesc(Pageable pageable, long totalElements) {
    long firstBlockIndexOfPage =
        totalElements - (long) pageable.getPageNumber() * pageable.getPageSize();
    Timestamp firstBlockTimeOfPage =
        blockRepository
            .findFirstByBlockNo(firstBlockIndexOfPage)
            .map(Block::getTime)
            .orElse(Timestamp.valueOf(MAX_TIME));

    List<Block> ebbBlocks = blockRepository.findGenesisAndEbbBlockInfo();
    int ebbBlockCountAfterFirstBlockOfPage =
        (int)
            ebbBlocks.stream()
                .filter(ebbBlock -> ebbBlock.getTime().after(firstBlockTimeOfPage))
                .count();

    long actualFirstBlockIndexOfPage;
    if (ebbBlockCountAfterFirstBlockOfPage > 0) {
      actualFirstBlockIndexOfPage =
          firstBlockIndexOfPage - (ebbBlocks.size() - ebbBlockCountAfterFirstBlockOfPage);
    } else {
      actualFirstBlockIndexOfPage = firstBlockIndexOfPage;
    }

    List<Block> blockList =
        customBlockRepository.findByBlockNoAndSpecifiedOffset(
            actualFirstBlockIndexOfPage, pageable.getPageSize(), Direction.DESC);

    if (blockList.isEmpty()) {
      return new PageImpl<>(Collections.emptyList(), pageable, totalElements);
    }

    Block firstBlockOfPage = blockList.get(0);
    Block lastBlockOfPage = blockList.get(blockList.size() - 1);

    List<Block> ebbBlockProjectionsInPage =
        ebbBlocks.stream()
            .filter(
                ebbBlock -> {
                  Timestamp blockTime = ebbBlock.getTime();
                  return blockTime.compareTo(firstBlockOfPage.getTime()) <= 0
                      && blockTime.compareTo(lastBlockOfPage.getTime()) >= 0;
                })
            .toList();

    blockList.addAll(ebbBlockProjectionsInPage);
    blockList.sort(Comparator.comparingLong(Block::getId).reversed());
    if (blockList.size() > pageable.getPageSize()) {
      blockList = blockList.subList(0, pageable.getPageSize());
    }
    return new PageImpl<>(blockList, pageable, totalElements);
  }

  @Override
  @Transactional(readOnly = true)
  public BaseFilterResponse<BlockFilterResponse> getBlockByEpoch(String no, Pageable pageable) {
    try {
      Integer epochNo = Integer.parseInt(no);
      Page<Block> blocks = blockRepository.findBlockByEpochNo(epochNo, pageable);
      return mapperBlockToBlockFilterResponse(blocks);
    } catch (NumberFormatException e) {
      throw new NoContentException(BusinessCode.EPOCH_NOT_FOUND);
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
    // get slot leader for block
    List<SlotLeader> slotLeaders =
        slotLeaderRepository.findByIdIn(
            blocks.getContent().stream().map(Block::getSlotLeaderId).collect(Collectors.toList()));
    Map<Long, SlotLeader> slotLeaderMap =
        slotLeaders.stream().collect(Collectors.toMap(BaseEntity::getId, Function.identity()));

    List<Tx> txList = txRepository.findByBlockIn(blocks.toList());

    // create map with key: block_id, value : total output of block
    Map<Long, BigInteger> blockTotalOutputMap =
        txList.stream()
            .collect(
                Collectors.groupingBy(
                    tx -> tx.getBlock().getId(),
                    Collectors.reducing(BigInteger.ZERO, Tx::getOutSum, BigInteger::add)));
    // create map with key: block_id, value : total fee of block
    Map<Long, BigInteger> blockTotalFeeMap =
        txList.stream()
            .collect(
                Collectors.groupingBy(
                    tx -> tx.getBlock().getId(),
                    Collectors.reducing(BigInteger.ZERO, Tx::getFee, BigInteger::add)));

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
