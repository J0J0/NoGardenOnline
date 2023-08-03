function getNoGardenOnlineLevels() {
  return  {
    s1:
      { cols: 3
      , rows: 3
      , blocked: [[2,2]]
      }
    ,
    s2:
      { cols: 4
      , rows: 3
      , blocked: [[2,1],[2,3],[3,1],[4,3]]
      }
    ,
    s3:
      { cols: 4
      , rows: 5
      , blocked: [[1,1],[2,3],[3,3],[4,2]]
      }
    ,
    m1:
      { cols: 7
      , rows: 6
      , blocked: [[2,2],[4,3]]
      }
    ,
    m2:
      { cols: 7
      , rows: 6
      , blocked: [[2,2],[4,2],[4,4],[5,2],[6,2],[3,4],[6,5],[7,5]]
      }
    ,
    l1:
      { cols: 10
      , rows: 7
      , blocked: [[2,3],[2,4],[3,4],[2,5],[3,5],[5,6],[6,6],
                  [10,1],[10,2],[10,3],[6,2],[7,2]]
      }
    ,
    l2:
      { cols: 10
      , rows: 7
      , blocked: [[1,7],[1,6],[2,7],[3,7],[5,6],[7,5],[5,5],[6,5],
                  [2,1],[3,1],[3,2],[8,3],[7,3],[8,2],[9,5],[9,6],
                  [10,6],[9,4],[3,3],[6,3]]
      }
  }
}
