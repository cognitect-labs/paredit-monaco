;; Copyright 2019 Cognitect, Inc. All Rights Reserved.
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;      http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS-IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

(ns cognitect.monaco.paredit)

;; integrates
;; https://robert.kra.hn/past-projects/paredit-js.html

;; TODO
;; cut/copy/yank/etc
;; nav expand/contract
;; wrapAround
;; indent

;; monaco terms:
;; editor model selection range position
;; paredit terms:
;; ast

(defn rebl-editor
  []
  js/window.editor)

(defn paredit-ast
  [src]
  (.parse js/paredit src))

(defn offset
  [model position]
  (.getOffsetAt model position))

(defn get-selection
  [editor]
  (let [model (.getModel editor)
        sel (.getSelection editor)]
    {:start (offset model (.getStartPosition sel))
     :end (offset model (.getEndPosition sel))
     :cur (offset model (.getPosition sel))}))

(defn apply-edits
  "takes paredit.js results and applies them to Monaco"
  [editor pinfo]
  (when pinfo
    (let [model (.getModel editor)
          range (fn [start end]
                  (.fromPositions js/monaco.Range
                                  (.getPositionAt model start)
                                  (.getPositionAt model end)))
          chgs (map (fn [[op i arg]]
                      (case op
                        "insert" #js {:range (range i i)
                                      :text arg}
                        "remove" #js {:range (range i (+ i arg))
                                      :text nil}))
                    (.-changes pinfo))]
      (.pushEditOperations model nil (into-array chgs) nil)
      (.setPosition editor (.getPositionAt model (.-newIndex pinfo))))))

(defn wrap-paredit-command
  [cmd]
  (fn [editor]
    (let [model (.getModel editor)
          src (.getValue model)
          pos (.getPosition editor)]
      (cmd {:editor editor
            :src src
            :ast (paredit-ast src)
            :selection (get-selection editor)}))))

(defn nav-thunk
  [paredit-cmd]
  (wrap-paredit-command
   (fn [{:keys [editor ast src selection]}]
     (let [model (.getModel editor)
           nidx (paredit-cmd ast (:cur selection))]
       (.setPosition editor (.getPositionAt model nidx))))))

(def actions
  [#js {:id "paredit-forward-slurp-sexp"
        :label "Slurp S-Expression Forward"
        :keybindings #js [(bit-or js/monaco.KeyMod.WinCtrl
                                  js/monaco.KeyMod.Shift
                                  js/monaco.KeyCode.KEY_0)]
        :run (wrap-paredit-command
              (fn [{:keys [editor ast src selection]}]
                (apply-edits editor (.slurpSexp js/paredit.editor ast src (:cur selection) #js {:backward false}))))}
   #js {:id "paredit-backward-slurp-sexp"
        :label "Slurp S-Expression Backward"
        :keybindings #js [(bit-or js/monaco.KeyMod.WinCtrl
                                  js/monaco.KeyMod.Shift
                                  js/monaco.KeyCode.KEY_9)]
        :run (wrap-paredit-command
              (fn [{:keys [editor ast src selection]}]
                (apply-edits editor (.slurpSexp js/paredit.editor ast src (:cur selection) #js {:backward true}))))}

   #js {:id "paredit-forward-barf-sexp"
        :label "Barf S-Expression Forward"
        :keybindings #js [(bit-or js/monaco.KeyMod.WinCtrl
                                  js/monaco.KeyMod.Shift
                                  js/monaco.KeyCode.US_CLOSE_SQUARE_BRACKET)]
        :run (wrap-paredit-command
              (fn [{:keys [editor ast src selection]}]
                (apply-edits editor (.barfSexp js/paredit.editor ast src (:cur selection) #js {:backward false}))))}

   #js {:id "paredit-backward-barf-sexp"
        :label "Barf S-Expression Backward"
        :keybindings #js [(bit-or js/monaco.KeyMod.WinCtrl
                                  js/monaco.KeyMod.Shift
                                  js/monaco.KeyCode.US_OPEN_SQUARE_BRACKET)]
        :run (wrap-paredit-command
              (fn [{:keys [editor ast src selection]}]
                (apply-edits editor (.barfSexp js/paredit.editor ast src (:cur selection) #js {:backward true}))))}

   #js {:id "paredit-kill"
        :label "Kill S-Expression Forward"
        :keybindings #js [(bit-or js/monaco.KeyMod.WinCtrl
                                  js/monaco.KeyCode.KEY_K)]
        :run (wrap-paredit-command
              (fn [{:keys [editor ast src selection]}]
                (apply-edits editor (.killSexp js/paredit.editor ast src (:cur selection) #js {:backward false}))))}

   #js {:id "paredit-kill-backward"
        :label "Kill S-Expression Backward"
        :keybindings #js [(bit-or js/monaco.KeyMod.WinCtrl
                                  js/monaco.KeyMod.Shift
                                  js/monaco.KeyCode.KEY_K)]
        :run (wrap-paredit-command
              (fn [{:keys [editor ast src selection]}]
                (apply-edits editor (.killSexp js/paredit.editor ast src (:cur selection) #js {:backward true}))))}

   #js {:id "paredit-split-sexp"
        :label "Split S-Expression"
        :keybindings #js [(bit-or js/monaco.KeyMod.Alt
                                  js/monaco.KeyMod.Shift
                                  js/monaco.KeyCode.KEY_S)]
        :run (wrap-paredit-command
              (fn [{:keys [editor ast src selection]}]
                (apply-edits editor (.splitSexp js/paredit.editor ast src (:cur selection) #js {}))))}

   #js {:id "paredit-splice-sexp"
        :label "Splice S-Expression"
        :keybindings #js [(bit-or js/monaco.KeyMod.Alt
                                  js/monaco.KeyCode.KEY_S)]
        :run (wrap-paredit-command
              (fn [{:keys [editor ast src selection]}]
                (apply-edits editor (.spliceSexp js/paredit.editor ast src (:cur selection) #js {}))))}

   #js {:id "paredit-forward"
        :label "Forward S-Expression"
        :keybindings #js [(bit-or js/monaco.KeyMod.Alt
                                  js/monaco.KeyCode.RightArrow)]
        :run (nav-thunk js/paredit.navigator.forwardSexp)}

   #js {:id "paredit-backward"
        :label "Backward S-Expression"
        :keybindings #js [(bit-or js/monaco.KeyMod.Alt
                                  js/monaco.KeyCode.LeftArrow)]
        :run (nav-thunk js/paredit.navigator.backwardSexp)}

   #js {:id "paredit-forward-down"
        :label "Forward Down S-Expression"
        :keybindings #js [(bit-or js/monaco.KeyMod.Alt
                                  js/monaco.KeyCode.DownArrow)]
        :run (nav-thunk js/paredit.navigator.forwardDownSexp)}

   #js {:id "paredit-backward-up"
        :label "Backward Up S-Expression"
        :keybindings #js [(bit-or js/monaco.KeyMod.Alt
                                  js/monaco.KeyCode.UpArrow)]
        :run (nav-thunk js/paredit.navigator.backwardUpSexp)}

   ;; not sure what paredit is doing here
   ;; consider paredit.editor.open() too
   #_ #js {:id "paredit-wrap-list"
        :label "Wrap as List"
        :keybindings #js [(bit-or js/monaco.KeyMod.Alt
                                  js/monaco.KeyMod.Shift
                                  js/monaco.KeyCode.KEY_K)]
        :run (wrap-paredit-command
              (fn [{:keys [editor ast src selection]}]
                (let [{:keys [cur start end]} selection]
                  (apply-edits editor (.wrapAround js/paredit.editor ast src cur "(" ")"
                                                   (if (= start end)
                                                     #js {}
                                                     #js {:endIdx end}))))))}])

(defn ^:export register-actions
  [editor]
  (doseq [a actions]
    (.addAction editor a)))

;; (register-actions (rebl-editor))

