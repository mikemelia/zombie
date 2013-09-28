(ns zombie.parallel)

(def processing-in-parallel? (atom false))

(defn in-parallel
  [function args]
  (do
    (mpi.MPI/Init args)
    (reset! processing-in-parallel? true)
    (function)
    (reset! processing-in-parallel? false)
    (mpi.MPI/Finalize)))

(defn parallel-if
  [function value]
  (if @processing-in-parallel? (function) value))

(defn processor-name
  []
  (parallel-if
    #(mpi.MPI/Get_processor_name)
    1))

(defn my-rank
  []
  (parallel-if #(.Rank mpi.MPI/COMM_WORLD) 1))

(defn size
  []
  (parallel-if #(.Size mpi.MPI/COMM_WORLD) 1))
