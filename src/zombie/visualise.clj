;; responsible for visualising the spread of the zombie virus
(ns zombie.visualise
  (import (javax.swing JFrame)
          (java.awt Graphics2D Color)
          (java.awt.image BufferedImage)))

(def factor (atom 0))

;; Create the frame for displaying the visualisation
(defn create-frame [x y downsize-factor]
(let [frame (JFrame. "ZOMBIES")]
  (reset! factor downsize-factor)
    (doto frame
      (.setSize (* @factor x) (* @factor y))
      (.setVisible true))
    frame))

;; draw a group of people (or zombies) on the graphics
(defn draw [graphics population]
  (doseq [location population] (.fillOval graphics (* @factor (-> location :x)) (* @factor (-> location :y)) @factor @factor)))

(defn draw-population
  [frame adults children elderly infected time]
  (let [graphics (.getGraphics frame)
        dimension (.getSize frame)
        width (.width dimension)
        height (.height dimension)
        image (BufferedImage. width height BufferedImage/TYPE_INT_RGB)
        image-graphics (.createGraphics image)]
    (.setTitle frame (str "ZOMBIES - spread of infection after " time " hours : " (count infected) " infected, " (count adults) "/" (count children) "/" (count elderly) " susceptible"))
    (.setPaint image-graphics Color/red)
    (draw image-graphics infected)
    (.setPaint image-graphics Color/gray)
    (draw image-graphics adults)
    (.setPaint image-graphics Color/darkGray)
    (draw image-graphics elderly)
    (.setPaint image-graphics Color/lightGray)
    (draw image-graphics children)
    (.drawImage graphics image 0 0 (reify java.awt.image.ImageObserver (imageUpdate [_ img infoflags x y width height] true)))))
