(ns zombie.parallel)

(def processing-in-parallel? (atom false))

(defn in-parallel
  [rank size function args]
  (do
    (mpi.MPI/Init args)
    (reset! processing-in-parallel? true)
    (reset! size (.Size mpi.MPI/COMM_WORLD))
    (reset! rank (.Rank mpi.MPI/COMM_WORLD))
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

(defn send-to
  [message destination]
  (.Isend mpi.MPI/COMM_WORLD (char-array message) 0 (count message) mpi.MPI/CHAR destination 1))


(defn receive-from
  ([] (receive-from 8192))
  ([count]
     (let [message (char-array count)]
       (.Recv mpi.MPI/COMM_WORLD message 0 count mpi.MPI/CHAR mpi.MPI/ANY_SOURCE mpi.MPI/ANY_TAG)
       (String. message))))
