package org.cardanofoundation.explorer.api.service.impl;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.function.Function;
import java.util.stream.Collectors;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.apache.commons.lang3.StringUtils;
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
import org.cardanofoundation.explorer.consumercommon.entity.BaseEntity_;
import org.cardanofoundation.explorer.consumercommon.entity.Block;
import org.cardanofoundation.explorer.consumercommon.entity.Block_;
import org.cardanofoundation.explorer.consumercommon.entity.SlotLeader;
import org.cardanofoundation.explorer.consumercommon.entity.Tx;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
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

    boolean sortFieldFlag = Block_.TIME.equals(sortField) || BaseEntity_.ID.equals(sortField);
    if (Direction.ASC.equals(direction) && Boolean.TRUE.equals(sortFieldFlag)) {
      blockPage = getBlockFilterListByTimeAsc(pageable, totalElements);
    } else if ((Direction.DESC.equals(direction) && Boolean.TRUE.equals(sortFieldFlag))
        || Objects.isNull(direction)
        || StringUtils.isEmpty(sortField)) {
      blockPage = getBlockFilterListByTimeDesc(pageable, totalElements);
    } else {
      blockPage = blockRepository.findAllBlock(pageable);
    }
    return mapperBlockToBlockFilterResponse(blockPage);
  }

  /**
   * Get block filter response order by time|id asc. Main idea: Find the first block id by page and
   * genesis|ebb block count. Then find the block list by first block id and limit.
   *
   * @param pageable
   * @param totalElements
   * @return block filter response
   */
  private Page<Block> getBlockFilterListByTimeAsc(Pageable pageable, long totalElements) {
    long firstBlockIndexOfPage = (long) pageable.getPageNumber() * pageable.getPageSize();
    long currentMaxBlockNo = blockRepository.findCurrentBlock().map(Long::valueOf).orElse(0L);
    long minBlockNo = blockRepository.findMinBlockNo();
    // if current max block no is less than first block index of page, then call
    // getBlocksAscByAFewPageAhead
    if (currentMaxBlockNo < firstBlockIndexOfPage) {
      return getBlocksAscByAFewPageAhead(
          pageable, totalElements, firstBlockIndexOfPage, currentMaxBlockNo);
    }
    // if blockNo start from 0, then the first block index of page should minus 1
    firstBlockIndexOfPage = firstBlockIndexOfPage - ((minBlockNo == 0) ? 1 : 0);
    // get first block id by first block no of page
    long firstBlockId =
        blockRepository.findFirstByBlockNo(firstBlockIndexOfPage).map(Block::getId).orElse(0L);

    // get genesis|ebb block count in front of first block id
    long ebbBlkCntInFrontOf =
        blockRepository.countGenesisAndEbbBlockInfoByBlockIdLessThan(firstBlockId).orElse(0L);

    long actualFirstBlockId;
    if (ebbBlkCntInFrontOf > 0) {
      actualFirstBlockId =
          blockRepository
              .findFirstByBlockNo(firstBlockIndexOfPage - ebbBlkCntInFrontOf + 1)
              .map(Block::getId)
              .orElse(0L);
    } else {
      actualFirstBlockId = firstBlockId;
    }

    List<Block> blockList =
        getBlocksByBlockIdAndLimit(pageable.getPageSize(), actualFirstBlockId, Direction.ASC);

    return new PageImpl<>(blockList, pageable, totalElements);
  }

  /**
   * Get block filter response ASC, based on last block of a few page ahead. Main idea: Find the
   * last block of a few page ahead
   *
   * @param pageable
   * @param totalElements
   * @param firstBlockIndexOfPage
   * @param currentMaxBlockNo
   * @return
   */
  private Page<Block> getBlocksAscByAFewPageAhead(
      Pageable pageable, long totalElements, long firstBlockIndexOfPage, long currentMaxBlockNo) {
    int numberOfAFewPageAhead =
        (int) (firstBlockIndexOfPage - currentMaxBlockNo) / pageable.getPageSize() + 1;

    Block lastBlockFromAFewPageAhead =
        getBlockFilterListByTimeAsc(
                PageRequest.of(
                    pageable.getPageNumber() - numberOfAFewPageAhead,
                    pageable.getPageSize(),
                    Direction.ASC,
                    BaseEntity_.ID),
                totalElements)
            .getContent()
            .get(pageable.getPageSize() - 1);

    long nextBlockId = blockRepository.findNextBlockId(lastBlockFromAFewPageAhead.getId());

    List<Block> blocksResponse =
        getBlocksByBlockIdAndLimit(
            numberOfAFewPageAhead * pageable.getPageSize(), nextBlockId, Direction.ASC);

    blocksResponse =
        blocksResponse.subList(
            Math.min(blocksResponse.size(), (numberOfAFewPageAhead - 1) * pageable.getPageSize()),
            Math.min(blocksResponse.size(), numberOfAFewPageAhead * pageable.getPageSize()));

    return new PageImpl<>(blocksResponse, pageable, totalElements);
  }

  private List<Block> getBlocksByBlockIdAndLimit(int limit, long blockId, Direction direction) {
    return customBlockRepository.findByBlockIdAndLimit(blockId, limit, direction);
  }

  /**
   * Get block filter response order by time|id desc. Main idea: Find the first block id by page and
   * genesis|ebb block count. Then find the block list by first block id and limit.
   *
   * @param pageable
   * @param totalElements
   * @return
   */
  private Page<Block> getBlockFilterListByTimeDesc(Pageable pageable, long totalElements) {
    long firstBlockIndexOfPage =
        totalElements - (long) pageable.getPageNumber() * pageable.getPageSize();

    long currentMaxBlockNo = blockRepository.findCurrentBlock().map(Long::valueOf).orElse(0L);
    if (firstBlockIndexOfPage > currentMaxBlockNo) {
      return getBlocksDescByFirstFewPage(pageable, totalElements, currentMaxBlockNo);
    }

    // get first block id by first block no of page
    long firstBlockId =
        blockRepository
            .findFirstByBlockNo(
                currentMaxBlockNo - (long) pageable.getPageNumber() * pageable.getPageSize())
            .map(Block::getId)
            .orElse(0L);

    // get total genesis|ebb block count
    long totalGenesisAndEbbBlockCnt =
        blockRepository.countGenesisAndEbbBlockInfoByBlockIdGreaterThan(0L).orElse(0L);

    // get genesis|ebb block count behind first block id
    long ebbBlkCntBehind =
        blockRepository.countGenesisAndEbbBlockInfoByBlockIdGreaterThan(firstBlockId).orElse(0L);

    // get actualFirstBlockId
    long actualFirstBlockId;
    if (ebbBlkCntBehind > 0) {
      actualFirstBlockId =
          blockRepository
              .findFirstByBlockNo(
                  firstBlockIndexOfPage - (totalGenesisAndEbbBlockCnt - ebbBlkCntBehind))
              .map(Block::getId)
              .orElse(0L);
    } else {
      actualFirstBlockId = firstBlockId;
    }

    List<Block> blockList =
        customBlockRepository.findByBlockIdAndLimit(
            actualFirstBlockId, pageable.getPageSize(), Direction.DESC);

    return new PageImpl<>(blockList, pageable, totalElements);
  }

  /**
   * Get block filter response DESC, based on first block of first few pages.
   *
   * @param pageable
   * @param totalElements
   * @param currentMaxBlockNo
   * @return
   */
  private PageImpl<Block> getBlocksDescByFirstFewPage(
      Pageable pageable, long totalElements, long currentMaxBlockNo) {
    long currentMaxBlockId =
        blockRepository.findFirstByBlockNo(currentMaxBlockNo).map(Block::getId).orElse(0L);

    List<Block> blocksResponse =
        getBlocksByBlockIdAndLimit(
            (pageable.getPageNumber() + 1) * pageable.getPageSize(),
            currentMaxBlockId,
            Direction.DESC);

    blocksResponse =
        blocksResponse.subList(
            Math.min(blocksResponse.size(), pageable.getPageNumber() * pageable.getPageSize()),
            Math.min(
                blocksResponse.size(), (pageable.getPageNumber() + 1) * pageable.getPageSize()));

    return new PageImpl<>(blocksResponse, pageable, totalElements);
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
