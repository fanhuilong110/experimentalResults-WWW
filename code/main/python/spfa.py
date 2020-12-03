import pandas as pd
import numpy as np
class CGraph:
    def spfa(self, vi, vj):
        """
        求最短路径
        :param vi:  顶点 vi
        :param vj:  顶点 vj
        :return:    距离 路径
        """
        if self._is_exist(vi) and self._is_exist(vj):
            vi_index = self.V.index(vi)
            vj_index = self.V.index(vj)
            if vi_index != vj_index:
                v_min = min(vi_index, vj_index)
                v_max = max(vi_index, vj_index)
                # 核心算法
                mat = self.mat.values          # 生成 二维矩阵 邻接矩阵 方便操作
                queue = Queue()                # 优化算法 队列
                d = [self.inf] * self.n        # 距离 数组
                flag = [False] * self.n        # 标记 是否在队列
                count = [0] * self.n           # 判断 是否存在负环
                pre_node = -np.ones(self.n, dtype=np.int8)  # 记录前驱节点，没有则用-1表示
                # 初始化
                d[v_min] = 0
                queue.put(v_min)
                flag[v_min] = True
                u = 0
                # 宽松开始
                while not queue.empty():
                    u = queue.get()                           # 出队
                    flag[u] = False                           # 出队列 改变标记
                    for i in range(self.n):
                        if mat[u][i] != self.inf:             # 判断是否 有连接
                            temp_d = d[u] + mat[u][i]
                            if temp_d < d[i]:
                                d[i] = temp_d                  # 松弛
                                pre_node[i] = u                # 松弛后就更新前驱节点
                                if not flag[i]:                # 查询是否入队
                                    queue.put(i)               # 入队
                                    flag[i] = True             # 标记入队
                                    count[i] += 1              # 入队 计数器 加一
                                    if count[i] > self.n:      # 如果计数器 大于 n 那么认为存在负环
                                        print('存在负环')
                                        return False
                pro_node = pre_node.tolist()
                pro_node.reverse()
                road_pro = [self.V[v_min]]
                for ij in pre_node:
                    if ij == -1:
                        break
                    else:
                        road_pro.append(self.V[ij])
                road_pro.append(self.V[v_max])
                new_li = list(set(road_pro))
                new_li.sort(key=road_pro.index)
                re = {'min_value': d[v_max], 'road': new_li}
                return re
            else:
                print('顶点相同')
                return False
        else:
            print('点不存在')
            return False