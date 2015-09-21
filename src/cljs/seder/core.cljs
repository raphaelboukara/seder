(ns seder.core
  (:use-macros [cljs.core.async.macros :only [go]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :refer [<! chan put!]]))

(enable-console-print!)

(defonce HEIGHT 75)
(defonce WIDTH 130)

(defn from-to [[y1 x1] [y2 x2]]
  (let [x-list (vec (take (- (max x2 x1) (min x2 x1)) (iterate inc (min x2 x1))))
        y-list (vec (take (- (max y2 y1) (min y2 y1)) (iterate inc (min y2 y1))))
        x-list (if (empty? x-list) (vec (repeat (count y-list) x1)) x-list)
        y-list (if (empty? y-list) (vec (repeat (count x-list) y1)) y-list)]
    (mapv #(do [%1 %2]) y-list x-list)))

(def app-state 
  (atom {:map (->> (repeat 0) (take WIDTH) vec repeat (take HEIGHT) vec)
         :steps [{:route [[55 48]]
                  :text "פשיטת בגדי חול"}
                 {:route [[55 48]]
                  :text "טבילה ראשונה"}
                 {:route [[55 48]]
                  :text "לבישת בגדי זהב"}
                 {:route [[55 48]]
                  :text "קידוש ראשון של ידיים ורגליים"}
                 {:route (partition 2 (flatten (vector (from-to [55 48] [55 63]) 
                                                       (from-to [55 62] [45 62])
                                                       (from-to [45 62] [45 66])
                                                       (from-to [45 65] [30 65])
                                                       (reverse (from-to [30 65] [30 57])))))
                  :text "שחיטת קרבן התמיד של שחר, קבלת דמו בכל י שרת, וזריקתו על המזבח "}
                 {:route (partition 2 (flatten (vector (from-to [30 57] [30 53]) 
                                                       (from-to [30 53] [36 53])
                                                       (from-to [35 53] [35 35])
                                                       (from-to [35 34] [38 34])
                                                       (reverse (from-to [37 34] [37 29])))))
                  :text "הטבת חמישה מנרות המנורה"}
                 {:route (partition 2 (flatten (vector (from-to [37 29] [37 34])
                                                       (reverse (from-to [38 34] [35 34])))))
                  :text "הקטרת קטורת הבוקר"}
                 {:route (partition 2 (flatten (vector (from-to [35 34] [38 34])
                                                       (reverse (from-to [37 34] [37 29])))))
                  :text "הטבת שני נרות המנורה הנותרים"}
                 {:route (partition 2 (flatten (vector (from-to [37 29] [37 34])
                                                       (from-to [38 34] [35 34])
                                                       (from-to [35 34] [35 54])
                                                       (from-to [35 53] [55 53])
                                                       (from-to [55 53] [55 60])
                                                       (reverse (from-to [55 59] [38 59])))))
                  :text "הקרבת ראש התמיד ואיבריו"}
                 {:route [[38 59]]
                  :text "הקטרת מנחת התמיד"}
                 {:route [[38 59]]
                  :text "הקרבת מנחת חביתין"}
                 {:route (partition 2 (flatten (vector (from-to [38 59] [55 59])
                                                       (from-to [55 60] [55 54])
                                                       (reverse (from-to [55 54] [42 54])))))
                  :text "ניסוך היין של קרבן התמיד"}
                 {:route (partition 2 (flatten (vector (from-to [42 54] [55 54])
                                                       (from-to [55 54] [55 60])
                                                       (reverse (from-to [55 59] [38 59])))))
                  :text "הקרבת שני כבשי מוסף שבת, ומנחתם"}
                 {:route (partition 2 (flatten (vector (from-to [38 59] [55 59])
                                                       (from-to [55 60] [55 53])
                                                       (from-to [55 53] [35 53])
                                                       (from-to [35 53] [35 36])
                                                       (from-to [35 36] [33 36])
                                                       (reverse (from-to [33 36] [33 29])))))
                  :text "עריכת לחם הפנים"}
                 {:route (partition 2 (flatten (vector (from-to [33 29] [33 36])
                                                       (from-to [33 36] [35 36])
                                                       (from-to [35 36] [35 53])
                                                       (from-to [35 53] [55 53])
                                                       (from-to [55 53] [55 60])
                                                       (reverse (from-to [55 59] [38 59])))))
                  :text "הקטרת שני בזיכי לחם הפנים"}
                 {:route [[38 59]]
                  :text "הקרבת הפר ושבעת הכבשים של מוסף היום לעולה, ומנחתם"}
                 {:route (partition 2 (flatten (vector (from-to [38 59] [55 59])
                                                       (from-to [55 59] [55 62])
                                                       (from-to [56 62] [48 62])
                                                       (from-to [48 62] [48 67])
                                                       (from-to [48 67] [54 67]))))
                  :text "קידוש שני של ידיים ורגליים"}
                 {:route [[54 67]]
                  :text "פשיטת בגדי זהב"}
                 {:route [[54 67]]
                  :text "טבילה שניה"}
                 {:route [[54 67]]
                  :text "לבישת בגדי לבן"}
                 {:route [[54 67]]
                  :text "קידוש שלישי של ידיים ורגליים"}
                 {:route (partition 2 (flatten (vector (from-to [54 67] [48 67])
                                                       (from-to [48 67] [48 65])
                                                       (from-to [48 65] [27 65])
                                                       (reverse (from-to [27 65] [27 59])))))
                  :text "סמיכה ראשונה, בשתי הידיים, על פר הכהן הגדול"}
                 {:route [[27 59]]
                  :text "הווידוי הראשון, על פר הכהן הגדול, על עוונותיו ועוונות אנשי ביתו כולל ההשתחוויה השניה בתפילתנו"}
                 {:route (partition 2 (flatten (vector (from-to [27 59] [27 65])
                                                       (from-to [27 65] [35 65])
                                                       (from-to [35 65] [35 70]))))
                  :text "העלאת הגורלות על שני השעירים, אחד לה' ואחד לעזאזל"}
                 {:route [[35 70]]
                  :text "קשירת לשון של זהורית בין שני קרני השעיר לעזאזל"}
                 {:route [[35 70]]
                  :text "העברת השעיר לעזאזל לכנגד שער המזרח"}
                 {:route [[35 70]]
                  :text "קשירת לשון של זהורית על צוואר השעיר לה'"}
                 {:route (partition 2 (flatten (vector (from-to [35 70] [35 65])
                                                       (from-to [35 65] [27 65])
                                                       (reverse (from-to [27 65] [27 59])))))
                  :text "סמיכה שניה, בשתי הידיים, על פר הכהן הגדול"}
                 {:route [[27 59]]
                  :text "הווידוי השני, על פר הכהן הגדול, על עוונותיו, עוונות אנשי ביתו ועוונות אחיו הכהנים כולל ההשתחוויה השלישית בתפילתנו"}
                 {:route [[27 59]]
                  :text "שחיטת פר הכהן הגדול וקבלת דמו במזרק"}
                 {:route (partition 2 (flatten (vector (from-to [27 59] [27 53])
                                                       (from-to [27 53] [36 53])
                                                       (reverse (from-to [35 53] [35 50])))))
                  :text "מסירת המזרק עם דם פר הכהן הגדול לכהן אחר, שממשיך למרס בו כדי שלא יקרוש"}
                 {:route (partition 2 (flatten (vector (from-to [35 50] [35 53])
                                                       (from-to [35 53] [54 53])
                                                       (from-to [54 53] [54 59])
                                                       (reverse (from-to [55 59] [38 59])))))
                  :text "לקיחת גחלים לוחשות במחתה העשויה זהב אדום"}
                 {:route (partition 2 (flatten (vector (from-to [38 59] [55 59])
                                                       (from-to [54 59] [54 53])
                                                       (from-to [54 53] [35 53])
                                                       (reverse (from-to [35 53] [35 50])))))
                  :text "הנחת המחתה"}
                 {:route [[35 50]]
                  :text "חפינת הקטורת, ממחתה שהובאה במיוחד לשם כך, ונתינתה אל כף-זהב"}
                 {:route [[35 50]]
                  :text "נטילת מחתת הגחלים ביד ימין וכף הקטורת ביד שמאל"}
                 {:route (partition 2 (flatten (vector (from-to [35 50] [35 35])
                                                       (from-to [35 35] [38 35])
                                                       (from-to [37 35] [37 28])
                                                       (from-to [37 28] [35 28])
                                                       (reverse (from-to [35 28] [35 22])))))
                  :text "כניסה ראשונה, והנחת המחתה בין בדי ארון העדות"}
                 {:route [[35 22]]
                  :text "חפינת כל הקטורת מהכף אל תוך שתי ידיו, ונתינתה על הגחלים"}
                 {:route (partition 2 (flatten (vector (from-to [35 22] [35 28]))))
                  :text "התפילה הקצרה"}
                 {:route (partition 2 (flatten (vector (from-to [35 27] [35 28])
                                                       (from-to [35 28] [37 28])
                                                       (from-to [37 28] [37 37])
                                                       (from-to [37 36] [35 36])
                                                       (from-to [35 36] [35 53])
                                                       (reverse (from-to [35 52] [32 52])))))
                  :text "נטילת דם פר הכהן הגדול מהכהן שמרס בו"}
                 {:route (partition 2 (flatten (vector (from-to [32 52] [35 52])
                                                       (from-to [35 53] [35 36])
                                                       (from-to [35 36] [37 36])
                                                       (from-to [37 37] [37 28])
                                                       (from-to [37 28] [35 28])
                                                       (reverse (from-to [35 28] [35 22])))))
                  :text "כניסה שניה, והזאת דם פר הכהן הגדול שמונה פעמים בין בדי הארון"}
                 {:route (partition 2 (flatten (vector (from-to [35 22] [35 28]))))
                  :text "הנחת המזרק עם דם פר הכהן הגדול על כַּן-זהב ראשון"}
                 {:route (partition 2 (flatten (vector (from-to [35 27] [35 28])
                                                       (from-to [35 28] [37 28])
                                                       (from-to [37 28] [37 37])
                                                       (from-to [37 36] [35 36])
                                                       (from-to [35 36] [35 53])
                                                       (from-to [35 52] [32 52])
                                                       (from-to [32 52] [32 60])
                                                       (reverse (from-to [33 60] [27 60])))))
                  :text "שחיטת שעיר החטאת וקבלת דמו במזרק"}
                 {:route (partition 2 (flatten (vector (from-to [27 60] [33 60])
                                                       (from-to [32 60] [32 52])
                                                       (from-to [32 52] [35 52])
                                                       (from-to [35 53] [35 36])
                                                       (from-to [35 36] [37 36])
                                                       (from-to [37 37] [37 28])
                                                       (from-to [37 28] [35 28])
                                                       (reverse (from-to [35 28] [35 22])))))
                  :text "כניסה שלישית, והזאת דם שעיר החטאת שמונה פעמים בין בדי הארון"}
                 {:route (partition 2 (flatten (vector (from-to [35 22] [35 28]))))
                  :text "הנחת המזרק עם דם שעיר החטאת על כַּן-זהב שני"}
                 {:route [[35 28]]
                  :text "נטילת המזרק עם דם פר הכהן הגדול מן הכַּן, והזאה ממנו שמונה פעמים על הפרוכת"}
                 {:route [[35 28]]
                  :text "הנחת המזרק עם דם פר הכהן הגדול על הכַּן הראשון, ונטילת המזרק עם דם שעיר החטאת מן הכַּן השני"}
                 {:route [[35 28]]
                  :text "הזאה מדם שעיר החטאת שמונה פעמים על הפרוכת"}
                 {:route [[35 28]]
                  :text "עירוי דם פר הכהן הגדול לתוך המזרק של דם שעיר החטאת, ואיחודם"}
                 {:route (partition 2 (flatten (vector (from-to [35 28] [37 28])
                                                       (from-to [37 28] [37 37])
                                                       (reverse (from-to [37 36] [35 36])))))
                  :text "הזאת דם התערובת על ארבע קרנות מזבח הזהב, ושבע פעמים על טהרו של מזבח הזהב"}
                 {:route (partition 2 (flatten (vector (from-to [35 36] [35 54])
                                                       (from-to [35 53] [44 53]))))
                  :text "שפיכת שיירי דם התערובת"}
                 {:route (partition 2 (flatten (vector (from-to [43 53] [54 53])
                                                       (from-to [54 53] [54 63])
                                                       (from-to [54 62] [47 62])
                                                       (from-to [47 62] [47 67])
                                                       (from-to [47 66] [35 66])
                                                       (from-to [35 66] [35 70]))))
                  :text "סמיכה בשתי הידיים על השעיר לעזאזל"}
                 {:route [[35 70]]
                  :text "הווידוי השלישי, על השעיר לעזאזל, על עוונות כל בית ישראל כולל ההשתחוויה הרביעית בתפילתנו"}
                 {:route [[35 70]]
                  :text "מסירת השעיר לעזאזל לאדם המוכן לכך, כדי שיוליכנו לארץ גזירה"}
                 {:route (partition 2 (flatten (vector (from-to [35 70] [35 65])
                                                       (from-to [35 65] [27 65])
                                                       (reverse (from-to [27 65] [27 59])))))
                  :text "הכנת פר הכהן הגדול ושעיר החטאת. את אימוריהם: להקטרה על המזבח החיצון. ואת בשרם: להוצאה לבית השריפה"}
                 {:route (partition 2 (flatten (vector (from-to [27 59] [27 69])
                                                       (from-to [27 69] [35 69])
                                                       (from-to [35 69] [35 82]))))
                  :text "קריאת פרשיות היום, וברכת שמונה הברכות"}
                 {:route (partition 2 (flatten (vector (from-to [35 82] [35 67])
                                                       (from-to [35 67] [54 67]))))
                  :text "קידוש רביעי של ידיים ורגליים"}
                 {:route [[54 67]]
                  :text "פשיטת בגדי לבן"}
                 {:route [[54 67]]
                  :text "טבילה שלישית"}
                 {:route [[54 67]]
                  :text "לבישת בגדי זהב"}
                 {:route [[54 67]]
                  :text "קידוש חמישי של ידיים ורגליים"}
                 {:route (partition 2 (flatten (vector (from-to [54 67] [48 67])
                                                       (from-to [48 67] [48 62])
                                                       (from-to [48 62] [56 62])
                                                       (from-to [55 62] [55 59])
                                                       (reverse (from-to [55 59] [38 59])))))
                  :text "הקרבת שעיר החטאת של מוסף היום"}
                 {:route [[38 59]]
                  :text "הקרבת איל הכהן הגדול ואיל העם, לעולה"}
                 {:route [[38 59]]
                  :text "הקטרת אימורי פר הכהן הגדול ושעיר החטאת"}
                 {:route [[38 59]]
                  :text "הקטרת מנחת איל הכהן הגדול ואיל העם"}
                 {:route (partition 2 (flatten (vector (from-to [38 59] [55 59])
                                                       (from-to [55 60] [55 54])
                                                       (reverse (from-to [55 54] [42 54])))))
                  :text "הקרבת נסכי כל קרבנות המוסף"}
                 {:route (partition 2 (flatten (vector (from-to [42 55] [42 53])
                                                       (from-to [42 53] [31 53])
                                                       (from-to [31 53] [31 60])
                                                       (reverse (from-to [31 59] [27 59])))))
                  :text "הקרבת קרבן התמיד של בין הערביים"}
                 {:route (partition 2 (flatten (vector (from-to [27 59] [27 65])
                                                       (from-to [27 65] [48 65])
                                                       (from-to [48 65] [48 67])
                                                       (from-to [48 67] [54 67]))))
                  :text "קידוש שישי של ידיים ורגליים"}
                 {:route [[54 67]]
                  :text "פשיטת בגדי זהב"}
                 {:route [[54 67]]
                  :text "טבילה רביעית"}
                 {:route [[54 67]]
                  :text "לבישת בגדי לבן"}
                 {:route [[54 67]]
                  :text "קידוש שביעי של ידיים ורגליים"}
                 {:route (partition 2 (flatten (vector (from-to [54 67] [49 67])
                                                       (from-to [49 68] [49 63])
                                                       (from-to [49 63] [55 63])
                                                       (from-to [55 64] [55 53])
                                                       (from-to [55 53] [35 53])
                                                       (from-to [35 54] [35 36])
                                                       (from-to [35 36] [37 36])
                                                       (from-to [37 37] [37 28])
                                                       (from-to [37 28] [35 28])
                                                       (reverse (from-to [35 28] [35 22])))))
                  :text "כניסה רביעית והוצאת הכף והמחתה"}
                 {:route (partition 2 (flatten (vector (from-to [35 22] [35 28])
                                                       (from-to [35 28] [37 28])
                                                       (from-to [37 28] [37 37])
                                                       (from-to [37 36] [35 36])
                                                       (from-to [35 36] [35 54])
                                                       (from-to [35 53] [55 53])
                                                       (from-to [55 53] [55 64])
                                                       (from-to [55 63] [49 63])
                                                       (from-to [49 63] [49 68])
                                                       (from-to [49 67] [54 67]))))
                  :text "קידוש שמיני של ידיים ורגליים"}
                 {:route [[54 67]]
                  :text "פשיטת בגדי לבן"}
                 {:route [[54 67]]
                  :text "טבילה חמישית"}
                 {:route [[54 67]]
                  :text "לבישת בגדי זהב"}
                 {:route [[54 67]]
                  :text "קידוש תשיעי של ידיים ורגליים"}
                 {:route (partition 2 (flatten (vector (from-to [54 67] [49 67])
                                                       (from-to [49 68] [49 63])
                                                       (from-to [49 63] [55 63])
                                                       (from-to [55 64] [55 53])
                                                       (from-to [55 53] [35 53])
                                                       (reverse (from-to [35 54] [35 35])))))
                  :text "הקטרת הקטורת של בין הערביים"}
                 {:route (partition 2 (flatten (vector (from-to [35 35] [35 54])
                                                       (from-to [35 53] [55 53])
                                                       (from-to [55 53] [55 60])
                                                       (reverse (from-to [55 59] [38 59])))))
                  :text "הקרבת מנחת התמיד ומוֹתר מנחת חביתין"}
                 {:route (partition 2 (flatten (vector (from-to [38 59] [55 59])
                                                       (from-to [55 60] [55 54])
                                                       (reverse (from-to [55 54] [42 54])))))
                  :text "ניסוך היין של קרבן התמיד"}
                 {:route (partition 2 (flatten (vector (from-to [42 55] [42 53])
                                                       (from-to [42 53] [35 53])
                                                       (from-to [35 53] [35 36])
                                                       (from-to [35 36] [37 36])
                                                       (reverse (from-to [37 37] [37 29])))))
                  :text "הדלקת נרות המנורה"}
                 {:route [[37 29]]
                  :text "השתחווייה"}
                 {:route (partition 2 (flatten (vector (from-to [37 29] [37 34])
                                                       (from-to [38 34] [35 34])
                                                       (from-to [35 34] [35 54])
                                                       (from-to [35 53] [55 53])
                                                       (from-to [55 53] [55 63])
                                                       (from-to [55 62] [49 62])
                                                       (from-to [49 62] [49 68])
                                                       (from-to [49 67] [54 67]))))
                  :text "קידוש עשירי של ידיים ורגליים"}
                 {:route [[54 67]]
                  :text "פשיטת בגדי זהב"}
                 {:route [[54 67]]
                  :text "לבישת בגדי עצמו"}]
         }))

(defn update-map-data [owner data step-id]
  (let [old-step-id (om/get-state owner :step-id)
        old-route (get-in data [:steps old-step-id :route])
        route (get-in data [:steps step-id :route])]
    (doseq [pt old-route] 
      (om/transact! data :map #(assoc-in % pt 0)))
    (doseq [pt (drop-last route)] 
      (om/transact! data :map #(assoc-in % pt 1)))
    (om/transact! data :map #(assoc-in % (last route) 2))
    (om/set-state! owner :step-id step-id)))

(defn navigate-view [data owner]
  (reify
    
    om/IInitState
    (init-state [_]
      {:step-id -1}) 
    
    om/IRenderState
    (render-state [_ {:keys [step-id]}]
      (dom/div #js {:className "navigation"}
        (dom/button 
          #js {:onClick #(update-map-data owner data (dec step-id))
               :disabled (not (pos? step-id))
               :className "button button-left"} 
          "-")
        (dom/button 
          #js {:onClick #(update-map-data owner data (inc step-id))
               :disabled (not (< step-id (dec (count (:steps data)))))
               :className "button button-right"}
          "+")
        (dom/span nil (get-in data [:steps step-id :text]))))))

(defn class-point [pt]
  (case pt
    1 "animated zoomIn active"
    2 "animated infinite bounceIn station"
    ""))

(defn line-view [line owner]
  (reify 
    om/IRender
    (render [_]
      (apply dom/tr nil 
        (mapv 
          #(dom/td #js {:className (class-point %)} nil) 
          line)))))

(defn map-view [data owner]
  (reify 
    om/IRender
    (render [_]
      (dom/div nil
        (apply dom/table nil
          (om/build-all line-view (:map data)))
        (om/build navigate-view data)))))



(defn main []
  (om/root
    map-view
    app-state
  {:target (. js/document (getElementById "app"))}))
