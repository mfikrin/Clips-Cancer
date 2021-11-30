;================================================================
; Author: Kelompok 51 Tubes 2 IF3170 2021/22 - AIme
; File: Cancer.clp
; Description: Program CLIPS untuk memprediksi penyakit kanker payudara sesuai kondisi
; ===============================================================

(deffunction ask-input()
    (bind ?inpoot (read))
    (if (or (eq (type ?inpoot) INTEGER) (eq (type ?inpoot) FLOAT))
       then (bind ?return ?inpoot)

    else 
        then (printout t "Ulangi input : ") (bind ?return (ask-input))
    )
    ?return
)


(deffacts initial-phase
   (phase start)
)

(defrule start
    ?phase <- (phase start)
=>
	(retract ?phase)
    (assert (phase continue))
    (printout t "   _____  .___                "crlf)
    (printout t "  /  _  \\ |   | _____   ____  "crlf)
    (printout t " /  /_\\  \\|   |/     \\_/ __ \\ "crlf)
    (printout t "/    |    \\   |  Y Y  \\  ___/ "crlf)
    (printout t "\\____|__  /___|__|_|  /\\___  >"crlf)
    (printout t "        \\/          \\/     \\/" crlf)

    (printout t crlf)


    (printout t "Program CLIPS untuk memprediksi penyakit kanker payudara sesuai kondisi" crlf)
    (printout t "Dibuat oleh Kelompok 51 AIme" crlf)
    (printout t "Input fitur atau kondisi (input numerik, int atau float)" crlf)
    (printout t "mean_concave_points : ")
    (bind ?inp (ask-input))
    (assert (mean-concave-Point ?inp))
    

    (if (> ?inp 0.05)
        then (assert (condition worst_perimeter)) (assert (prev_condition mean_concave_points))
    else
        then (assert (condition worst_radius)) (assert (prev_condition mean_concave_points))
    )
)

(defrule worst_radius
    ?phase <- (phase continue)
    ?condition <- (condition worst_radius)
    ?prev <- (prev_condition mean_concave_points)    
=>
    (retract ?condition ?prev)
    (printout t "worst_radius : ")
    (bind ?inp (ask-input))
    (assert (worst_radius ?inp))
    
    (if (<= ?inp 16.83)
        then (assert (condition radius_error)) (assert (prev_condition worst_radius))
    else 
        then (assert (condition mean_texture)) (assert (prev_condition worst_radius))
    )  
)

(defrule radius_error
    ?phase <- (phase continue)
    ?condition <- (condition radius_error)  
    ?prev <- (prev_condition worst_radius)  
=>
    (retract ?condition ?prev)
    (printout t "radius_error : ")
    (bind ?inp (ask-input))
    (assert (radius_error ?inp))
    
    (if (<= ?inp 0.63)
        then (assert (condition worst_texture)) (assert (prev_condition radius_error))
    else 
        then (assert (condition mean_smoothness)) (assert (prev_condition radius_error))
    )  
)

(defrule worst_area
    ?phase <- (phase continue)
    ?condition <- (condition worst_area)    
    ?prev <- (prev_condition worst_texture)
=>
    (retract ?condition ?prev)
    (printout t "worst_area : ")
    (bind ?inp (ask-input))
    (assert (worst_area ?inp))
    
    (if (<= ?inp 641.60)
        then (retract ?phase) (assert (phase end)) (assert (cancer yes))
    else 
        then (assert (condition mean_radius)) (assert (prev_condition worst_area))
    )  
)

(defrule mean_smoothness
    ?phase <- (phase continue)
    ?condition <- (condition mean_smoothness)  
    ?prev <- (prev_condition radius_error)  
=>
    (retract ?condition ?prev)
    (printout t "mean_smoothness : ")
    (bind ?inp (ask-input))
    (assert (mean_smoothness ?inp))
    
    (if (<= ?inp 0.09)
        then (retract ?phase) (assert (phase end)) (assert (cancer yes))
    else 
        then (retract ?phase) (assert (phase end)) (assert (cancer no))
    )  
)

(defrule mean_texture
    ?phase <- (phase continue)
    ?condition <- (condition mean_texture) 
    ?prev <- (prev_condition ?var_prev&worst_radius | mean_radius)   
=>
    (retract ?condition)
    (printout t "mean_texture : ")
    (bind ?inp (ask-input))
    (assert (mean_texture ?inp))
    
    (if (eq ?var_prev worst_radius)
        then
            (if (<= ?inp 16.19)
                then (retract ?phase ?prev) (assert (phase end)) (assert (cancer yes))
            else 
                then (retract ?prev) (assert (condition concave_points_error)) (assert (prev_condition mean_texture))
            )
    else (if (eq ?var_prev mean_radius)
            then
                (if (<= ?inp 28.79)
                    then (retract ?phase ?prev) (assert (phase end)) (assert (cancer no))
                else 
                    then (retract ?phase ?prev) (assert (phase end)) (assert (cancer yes))
                )
        )
    )
)

(defrule concave_points_error
    ?phase <- (phase continue)
    ?condition <- (condition concave_points_error)
    ?prev <- (prev_condition mean_texture)
=>
    (retract ?condition ?prev)
    (printout t "concave_points_error : ")
    (bind ?inp (ask-input))
    (assert (concave_points_error ?inp))
    
    (if (<= ?inp 0.01)
        then (retract ?phase) (assert (phase end)) (assert (cancer no))
    else 
        then (retract ?phase) (assert (phase end)) (assert (cancer yes))
    )  
)

(defrule worst_perimeter
    ?phase <- (phase continue)
    ?condition <- (condition worst_perimeter)
    ?prev <- (prev_condition mean_concave_points)
=>
    (retract ?condition ?prev)
    (printout t "worst_perimeter : ")
    (bind ?inp (ask-input))
    (assert (worst_perimeter ?inp))
    
    (if (<= ?inp 114.45)
        then (assert (condition worst_texture)) (assert (prev_condition worst_perimeter))
    else 
        then (retract ?phase) (assert (phase end)) (assert (cancer no))
    )
       
)

(defrule worst_texture
    ?phase <- (phase continue)
    ?condition <- (condition worst_texture)
    ?prev <- (prev_condition ?var_prev&radius_error | worst_perimeter)       
=>
    (retract ?condition)
    (printout t "worst_texture : ")
    (bind ?inp (ask-input))
    (assert (worst_texture ?inp))
    
    (if (eq ?var_prev radius_error)
        then
            (if (<= ?inp 30.15)
                then (retract ?phase ?prev) (assert (phase end)) (assert (cancer yes))
            else 
                then (retract ?prev) (assert (condition worst_area)) (assert (prev_condition worst_texture))
            )
    else (if (eq ?var_prev worst_perimeter)
            then
                (if (<= ?inp 25.65)
                    then (retract ?prev) (assert (condition worst_concave_points)) (assert (prev_condition worst_texture))
                else 
                    then (retract ?prev) (assert (condition perimeter_error)) (assert (prev_condition worst_texture))
                )
        )
    )   
)

(defrule worst_concave_points
    ?phase <- (phase continue)
    ?condition <- (condition worst_concave_points)
    ?prev <- (prev_condition worst_texture)    
=>
    (retract ?condition ?prev)
    (printout t "worst_concave_points : ")
    (bind ?inp (ask-input))
    
    
    (if (<= ?inp 0.17)
        then (retract ?phase) (assert (phase end)) (assert (cancer yes))
    else 
        then (retract ?phase) (assert (phase end)) (assert (cancer no))
    )
    
)

(defrule perimeter_error
    ?phase <- (phase continue)
    ?condition <- (condition perimeter_error)    
    ?prev <- (prev_condition worst_texture)
=>
    (retract ?condition ?prev)
    (printout t "perimeter_error : ")
    (bind ?inp (ask-input))
    (assert (perimeter_error ?inp))
    
    (if (<= ?inp 1.56)
        then (assert (condition mean_radius)) (assert (prev_condition perimeter_error))
    else 
        then (retract ?phase) (assert (phase end)) (assert (cancer no))
    )
    
)

(defrule mean_radius
    ?phase <- (phase continue)
    ?condition <- (condition mean_radius)
    ?prev <- (prev_condition ?var_prev&worst_area | perimeter_error) 
=>
    (retract ?condition)
    (printout t "mean_radius : ")
    (bind ?inp (ask-input))
     
    (if (eq ?var_prev perimeter_error)
        then
            (if (<= ?inp 13.34)
                then (retract ?phase ?prev) (assert (phase end)) (assert (cancer no))
            else 
                then (retract ?phase ?prev) (assert (phase end)) (assert (cancer yes))
            )
    else (if (eq ?var_prev worst_area)
            then
                (if (<= ?inp 13.45)
                    then (retract ?prev) (assert (condition mean_texture)) (assert (prev_condition mean_radius))
                else 
                    then (retract ?phase ?prev) (assert (phase end)) (assert (cancer yes))
                )
        )
    )
)

(defrule print_info
    ?phase <- (phase end)
    ?cancer <- (cancer ?var_cancer&yes | no)

=> 
    (retract ?phase)   
    (if (eq ?var_cancer yes)
        then
            (retract ?cancer) (printout t "Hasil Prediksi = Terprediksi Kanker Payudara" crlf)
    else (if (eq ?var_cancer no)
        then
            (retract ?cancer) (printout t "Hasil Prediksi = Tidak Terprediksi Kanker Payudara" crlf)
        )
    )

    (printout t "Masih ingin lanjut ? y/n : " )
    (bind ?inp (read))
    (printout t crlf)
    (if (or (eq ?inp y) (eq ?inp Y))
        then (assert (phase start))
    
    else
        then (printout t "Terima kasih telah menggunakan aplikasi kami." crlf)
    )
)



