package org.cardanofoundation.explorer.api.repository;

import java.util.List;
import org.cardanofoundation.explorer.consumercommon.entity.Block;
import org.springframework.data.domain.Sort.Direction;

public interface CustomBlockRepository {

  List<Block> findByBlockNoAndSpecifiedOffset(Long blockNo, int limit, Direction direction);
}
