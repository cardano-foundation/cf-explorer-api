package org.cardanofoundation.explorer.api.repository.ledgersync;

import java.util.List;
import org.cardanofoundation.explorer.consumercommon.entity.Block;
import org.springframework.data.domain.Sort.Direction;

public interface CustomBlockRepository {

  /**
   * Find blocks by block_id, limit and direction. Based on direction, the query will return blocks
   * with id greater or less than the given blockId with predefined limit.
   *
   * @param blockId block id
   * @param limit limit of results
   * @param direction ASC | DESC
   * @return list of blocks
   */
  List<Block> findByBlockIdAndLimit(Long blockId, int limit, Direction direction);
}
