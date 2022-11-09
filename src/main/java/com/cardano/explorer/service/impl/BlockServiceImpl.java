package com.cardano.explorer.service.impl;

import com.cardano.explorer.entity.Block;
import com.cardano.explorer.entity.Tx;
import com.cardano.explorer.exception.BusinessCode;
import com.cardano.explorer.exception.BusinessException;
import com.cardano.explorer.mapper.BlockMapper;
import com.cardano.explorer.model.BaseFilterResponse;
import com.cardano.explorer.model.BlockFilterResponse;
import com.cardano.explorer.model.BlockResponse;
import com.cardano.explorer.repository.BlockRepository;
import com.cardano.explorer.repository.TxRepository;
import com.cardano.explorer.service.BlockService;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@RequiredArgsConstructor
@Log4j2
public class BlockServiceImpl implements BlockService {

  private final BlockRepository blockRepository;

  private final TxRepository txRepository;

  private final BlockMapper blockMapper;

  @Override
  @Transactional(readOnly = true)
  public BlockResponse getBlockDetail(Integer no) {
    Block block = blockRepository.findByBlockNo(no).orElseThrow(
        () -> new BusinessException(BusinessCode.NOT_FOUND)
    );
    return blockMapper.blockToBlockResponse(block);
  }

  @Override
  @Transactional(readOnly = true)
  public BaseFilterResponse<BlockFilterResponse> getAllBlock(Pageable pageable) {
    Integer count = blockRepository.countAll().orElse(0);
    List<Block> blocks = new ArrayList<>();
    if(count > 0) {
      blocks = blockRepository.findAllBlock(pageable);
    }
    Page<Block> page = new PageImpl<>(blocks, pageable , count);
//    Page<Block> page = blockRepository.findAll(pageable);
    BaseFilterResponse<BlockFilterResponse> response = mapperBlockToBlockFilterResponse(page);
    response.setCurrentPage(pageable.getPageNumber());
    return response;
  }

  @Override
  @Transactional(readOnly = true)
  public BaseFilterResponse<BlockFilterResponse> getBlockByEpochNo(Integer epochNo,
      Pageable pageable) {

    Page<Block> blocks = blockRepository.findByEpochNo(epochNo, pageable);
    BaseFilterResponse<BlockFilterResponse> response = mapperBlockToBlockFilterResponse(blocks);
    response.setCurrentPage(pageable.getPageNumber());
    return response;
  }

  /**
   * Mapping from block entity to block response dto
   *
   * @param blocks list block entity
   * @return list block information in this page
   */
  private BaseFilterResponse<BlockFilterResponse> mapperBlockToBlockFilterResponse(
      Page<Block> blocks) {
    BaseFilterResponse<BlockFilterResponse> response = new BaseFilterResponse<>();
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
      BlockFilterResponse blockResponse = blockMapper.blockToBlockFilterResponse(block);
      var totalOutput = blockTotalOutputMap.get(block.getId());
      var totalFees = blockTotalFeeMap.get(block.getId());
      blockResponse.setTotalOutput(Objects.requireNonNullElse(totalOutput, BigDecimal.ZERO));
      blockResponse.setTotalFees(Objects.requireNonNullElse(totalFees, BigDecimal.ZERO));
      blockFilterResponseList.add(blockResponse);
    }
    response.setData(blockFilterResponseList);
    response.setTotalItems(blocks.getTotalElements());
    response.setTotalPages(blocks.getTotalPages());
    return response;
  }
}
