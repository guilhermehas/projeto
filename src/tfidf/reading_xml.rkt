(require xml)
(require txexpr)

; retorna o texto da primeira questão
; o "filter string?" é para retirar os caracteres inadequados, quando o XML original for refeito poderemos retirar esta parte

(string-join (filter string? (rest (rest
                (third (let* ((fp (open-input-file "2010-01.xml" #:mode 'text))
                              (arq (xml->xexpr (document-element (read-xml fp))))
                              (L (get-elements arq)))
                         (first L)))))))
