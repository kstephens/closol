(ns closol.matrix
  (:require
    [closol.v :refer :all])
  (:import
    (java.awt Color Dimension BorderLayout Image)
    (java.awt.image BufferedImage)
    (javax.swing JPanel JFrame JOptionPane JButton JLabel)
    (java.awt.event KeyListener)
    (java.io File)
    (javax.imageio ImageIO)
    )
  (:gen-class))

(defrecord Matrix [data width height])
(defn matrix [w h fij]
  (Matrix.
    (vec (map
           (fn [j]
             (vec (map (fn [i] (fij i j)) (range 0 w))))
           (range 0 h)))
           w h))

(defn matrix-fxy [w h xmin xmax ymin ymax fxy]
  (matrix w h
    (fn [i j]
      (let [ xf (v-lerp-1 (double i) 0.0 (double (- w 1)))
             yf (v-lerp-1 (double j) 0.0 (double (- h 1)))
             x (v-lerp xf (double xmin) (double xmax))
             y (v-lerp yf (double ymin) (double ymax)) ]
        (fxy x y)))))

(defn matrix-data   [m] (.data m))
(defn matrix-width  [m] (.width m))
(defn matrix-height [m] (.height m))

(defn matrix-get [m i j]
  (get (get (.data m) j) i))
 
(defn matrix-map [m fvij]
  (matrix (.width m) (.height m)
    (fn [i j] (fvij (matrix-get m i j) i j))))
 
(defn matrix-min-max [m]
  (let [ min (reduce fv-min (map #(reduce fv-min %1) (.data m)))
         max (reduce fv-max (map #(reduce fv-max %1) (.data m))) ]
    [min max]))

(defn matrix-fix-float [m]
  (matrix-map m (fn [v i j] (v-v v))))

 ;; Maps elements:
 ;; [min, max] => [vmin, vmax].
(defn matrix-range [m vmin vmax & opts]
  (let [ mmin-max (matrix-min-max m)
         mmin     (first  mmin-max)
         mmax     (second mmin-max)
         mscale   (if (= mmin mmax) 1.0 (double (- mmax mmin)))
         fvij     (if (empty? opts) (fn [v i j] v) (first opts)) ]
    (matrix-map m 
      (fn [v i j]
        (fvij (v-lerp (/ (- v mmin) mscale) vmin vmax) i j)))))

(defn matrix-zero? [m]
  (every? #(every? zero? %1) (.data m)))

(defn matrix-graymap [m]
  (matrix-range m 0.0 255.9999 (fn [v i j] (v-int (v-v v)))))

(defn matrix-image [m]
  (let [ image (BufferedImage. (.width m) (.height m) BufferedImage/TYPE_INT_ARGB)
         g (.getGraphics image) ]
    (matrix-map m (fn [v i j]
                    (let [ c1 (v-int (v-1 v)) c2 (v-int (v-2 v)) c3 (v-int (v-3 v)) ]
                      ;; (println (map type [c1 c2 c3]))
                      (.setColor g (new Color c1 c2 c3)))
                    (.fillRect g j i 1 1)))
    image))

(defn image-to-file [img file]
  (ImageIO/write img "png" (File. file)))

