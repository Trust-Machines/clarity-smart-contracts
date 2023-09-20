;; Copyright: (c) 2023 by Nassau Machines Inc.
;; This file is part of Trust Machines.
;; Your Project is free software. You may redistribute or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License or
;; (at your option) any later version.
;;
;; Your Project is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY, including without the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Your Project. If not, see <http://www.gnu.org/licenses/>.
;;
;; Based on Fungible Token, by @friedger
;; https://github.com/friedger/clarity-smart-contracts/blob/main/contracts/tokens/fungible-token.clar

;; Composable Fungible Token

(define-fungible-token fungible-token)

;; Storage
(define-map allowances
  ((spender principal) (owner principal))
  ((allowance uint)))
(define-data-var total-supply uint u21000000000)

;; Internals

;; Total number of tokens in existence.
(define-private (get-total-supply)
  (var-get total-supply))

;; Gets the amount of tokens that an owner allowed to a spender.
(define-private (allowance (spender principal) (owner principal))
  (let ((allowance-val (default-to u0 (map-get? allowances (tuple (spender owner))))))
    allowance-val
  )
)


;; Transfers tokens to a specified principal.
(define-private (transfer (amount uint) (sender principal) (recipient principal) )
  (match (ft-transfer? fungible-token amount sender recipient)
    result (ok true)
    error (err false))
)

;; Decrease allowance of a specified spender.
(define-private (decrease-allowance (amount uint) (spender principal) (owner principal))
  (let ((allowance-val (allowance spender owner)))
    (if (or (> amount allowance-val) (<= amount u0))
      true
      (begin
        (map-set allowances
          ((spender spender) (owner owner))
          ((allowance-val (- allowance-val amount))))
        true)))
        )
        
;; Internal - Increase allowance of a specified spender.
(define-private (increase-allowance (amount uint) (spender principal) (owner principal))
  (let ((allowance-val (allowance spender owner)))
    (if (<= amount u0)
      false
      (begin
         (map-set allowances
          ((spender spender) (owner owner))
          ((allowance-val (+ allowance-val amount))))
        true))))

;; Public functions

;; Transfers tokens to a specified principal.
(define-public (transfer-token (amount uint) (recipient principal) )
  (transfer amount tx-sender recipient)
)

;; Transfers tokens to a specified principal, performed by a spender
(define-public (transfer-from (amount uint) (owner principal) (recipient principal) )
  (let ((allowance (allowance tx-sender owner)))
    (begin
      (if (or (> amount allowance-val) (<= amount u0))
        (err false)
        (if (and
            (unwrap! (transfer amount owner recipient) (err false))
            (decrease-allowance amount tx-sender owner))
        (ok true)
        (err false)))))
)

;; Update the allowance for a given spender
(define-public (approve (amount uint) (spender principal) )
  (if (and (> amount u0)
           (print (increase-allowance amount spender tx-sender )))
      (ok amount)
      (err false)))

;; Revoke a given spender
(define-public (revoke (spender principal))
  (let ((allowance-val (allowance spender tx-sender)))
    (if (and (> allowance-val u0)
             (decrease-allowance allowance-val spender tx-sender))
        (ok 0)
        (err false))))

;; Retrieve the human-readable name of the token
(define-public (get-name)
  "MyToken"
)

;; Retrieve the ticker symbol of the token
(define-public (get-symbol)
  "MTK"
)

;; Retrieve the number of decimals used
(define-public (get-decimals)
  8 ;; Assuming 8 decimals
)

;; Retrieve the balance of a specific principal
(define-public (get-balance (owner principal))
  (ft-get-balance fungible-token owner)
)

;; Retrieve the current total supply
(define-public (get-total-supply)
  (var-get total-supply)
)

;; Retrieve an example URI that represents metadata of this token
(define-public (get-token-uri)
  "ipfs://QmXwNHQ1BmE2hLRykAMMxjsmdeDGFSFg63KDMBUhtcMcKc"
)

;; Mint new tokens.
(define-private (mint (amount uint) (account principal))
  (if (<= amount u0)
      (err false)
      (begin
        (var-set total-supply (+ (var-get total-supply) amount))
        (ft-mint? fungible-token amount account)
        (ok amount))))

;; Initialize the contract
(begin
  (mint u200 'ST398K1WZTBVY6FE2YEHM6HP20VSNVSSPJTW0D53M)
  (mint u100 'ST1JDEC841ZDWN9CKXKJMDQGP5TW1AM10B7EV0DV9))


