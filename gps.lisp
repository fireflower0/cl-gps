;; 使用可能なオペレータのリスト
(defvar *ops* nil)

;; preconds、add-listおよびdel-Hstでの操作
(defstruct op
  (action nil)
  (preconds nil)
  (add-list nil)
  (del-list nil))

(defparameter *school-ops*
  (list
   (make-op :action   'drive-son-to-school
            :preconds '(son-at-home car-works)
            :add-list '(son-at-school)
            :del-list '(son-at-home))
   (make-op :action   'shop-installs-battery
            :preconds '(car-needs-battery shop-knows-problem shop-has-money)
            :add-list '(car-works))
   (make-op :action   'tell-shop-problem
            :preconds '(in-communication-with-shop)
            :add-list '(shop-knows-problem))
   (make-op :action   'telephone-shop
            :preconds '(know-phone-number)
            :add-list '(in-communication-with-shop))
   (make-op :action   'look-up-number
            :preconds '(have-phone-book)
            :add-list '(know-phone-number))
   (make-op :action   'give-shop-money
            :preconds '(have-money)
            :add-list '(shop-has-money)
            :del-list '(have-money))))

;; dbgで使用される識別子
(defvar *dbg-ids* nil)

;;; Debug機能

(defun gps-debug (&rest ids)
  (setf *dbg-ids* (union ids *dbg-ids*)))

(defun gps-undebug (&rest ids)
  (setf *dbg-ids* (if (null ids)
                      nil
                      (set-difference *dbg-ids* ids))))

;; DEBUG ID が指定されている場合、インデントされたデバッグ情報を表示します。
(defun dbg-indent (id indent format-string &rest args)
  (when (member id *dbg-ids*)
    (fresh-line *debug-io*)
    (dotimes (i indent) (princ " " *debug-io*))
    (apply #'format *debug-io* format-string args)))

;;; 補助機能

;; 一致するすべての要素のリスト
(defun find-all (item sequence
                 &rest keyword-args
                 &key (test #'eql) test-not &allow-other-keys)
  (if test-not
      (apply #'remove item sequence
             :test-not (complement test-not) keyword-args)
      (apply #'remove item sequence
             :test (complement test) keyword-args)))

;; 要素がリストのメンバーと等しいかどうかをテストします
(defun member-equal (item list)
  (member item list :test #'equal))

;; 引数は与えられたAtomで始まるリストか判定
(defun starts-with (list x)
  (and (consp list) (eql (first list) x)))

;; 条件は実行形式か判定
(defun executing-p (x)
  (starts-with x 'executing))

;; 実行規則を使用するように演算子を変換します
(defun convert-op (op)
  (unless (some #'executing-p (op-add-list op))
    (push (list 'executing (op-action op)) (op-add-list op)))
  op)

;; オペレータを作成
(defun op (action &key preconds add-list del-list)
  (convert-op
   (make-op :action   action
            :preconds preconds
            :add-list add-list
            :del-list del-list)))

;; オペレータのリストを使用
(defun use (oplist)
  (length (setf *ops* oplist)))

;;; 主機能

;; オペレータが目標に適しているかどうかを決定する
(defun appropriate-p (goal op)
  (member-equal goal (op-add-list op)))

;; 現在の状態にオペレータを適用します
(defun apply-op (state goal op goal-stack)
  (dbg-indent :gps (length goal-stack) "Consider: ~a" (op-action op))
  (let ((state2 (achieve-all state (op-preconds op)
                             (cons goal goal-stack))))
    (unless (null state2)
      (dbg-indent :gps (length goal-stack) "Action: ~a" (op-action op))
      (append (remove-if #'(lambda (x)
                             (member-equal x (op-del-list op)))
                         state2)
              (op-add-list op)))))

;; 個々の目標を達成する
(defun achieve (state goal goal-stack)
  (dbg-indent :gps (length goal-stack) "Goal: ~a" goal)
  (cond ((member-equal goal state) state)
        ((member-equal goal goal-stack) nil)
        (t (some #'(lambda (op) (apply-op state goal op goal-stack))
                 (find-all goal *ops* :test #'appropriate-p)))))

;; 目標のリストを達成する
(defun achieve-all (state goals goal-stack)
  (let ((current-state state))
    (if (and (every #'(lambda (g)
                        (setf current-state
                              (achieve current-state g goal-stack)))
                    goals)
             (subsetp goals current-state :test #'equal))
        current-state)))

;;; トップレベル関数
;; オペレータのリストを使用して状態から目的を解決する
(defun GPS (state goals &optional (ops *ops*))
  (let ((old-ops *ops*))
    (setf *ops* ops)
    (let ((result (remove-if #'atom (achieve-all (cons '(start) state) goals nil))))
      (setf *ops* old-ops)
      result)))
