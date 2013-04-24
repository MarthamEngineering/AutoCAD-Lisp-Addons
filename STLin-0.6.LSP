;;; ---------------------------------------- ;;;
;;;
;;;            STLin AutoLisp Routine
;;; 	        Import ASCII STL File	          
;;;
;;; ---------------------------------------- ;;;

;;	Copyright (C) 2013  Martham Engineering Ltd
;;	This program is free software: you can redistribute it and/or modify
;;	it under the terms of the GNU General Public License as published by
;;	the Free Software Foundation, either version 3 of the License, or
;;	(at your option) any later version.
;;
;;    	This program is distributed in the hope that it will be useful,
;;    	but WITHOUT ANY WARRANTY; without even the implied warranty of
;;    	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;    	GNU General Public License for more details.
;;
;;    	You should have received a copy of the GNU General Public License
;;    	along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;	To Run:
;;	Make sure the file is in the AUtoCAD path and type stlin on the AutoCAD command line


(defun c:stlin  (/  *error* file)

;;; ---------------------------------------- ;;;
;;;
;;;         Error Handling
;;;
;;; ---------------------------------------- ;;;

    (defun *error* ( msg )
	
	;;reset command line output
	(setvar "CMDECHO" 1)
	(setvar "osmode" snapMode)
        
	(princ "\nError While Importing STL: ")
        (princ msg)
        (princ "\n")
	(princ)
    )

;;; ---------------------------------------- ;;;
;;;
;;;         Main Function
;;;
;;; ---------------------------------------- ;;;

(setq file (getfiled "" "" "stl" 8)) ;; open the file dialog and select a file

	(setq 
	    f (open file "r") ;;open the .STL file for processing
		pt1 nil
		pt2 nil
		pt3 nil
		counter 0 ;; set a variable to count the number of line of data being processed.
		snapMode (getvar "osmode") ;; get the current snap mode
	)
	
	;;Turn object snap off
	(setvar "osmode" 0)
	;;turn off command echo
	(setvar "CMDECHO" 0)
	

	;;open a log file
	(if (setq logfile (open "c:/lisplog.txt" "w"))
		(progn
		(write-line "\n---------- \nStart of data logging \n----------" logfile)
        ;;(princ "\nLog File Opened")
        )
		(princ "\n Error - Log File was not opened.")
    )
	
	
	(setq dataline (read-line f))		
	(while (/= dataline nil)
	
	;;remove all blank characters before data
	(while (= (substr dataline 1 1)  " ")	
	(setq dataline (substr dataline 2))
	)
		(if (= (substr dataline 1 5) "solid")	
			(progn
			(princ "\n----- ASCI STL Importer -----") 
			(princ (strcat "\nProcessing ASCI STL file: " (substr dataline 6) ))
			(write-line (strcat "\nProcessing ASCI STL file: " (substr dataline 6)) logfile)
			(princ "\n")
			)
		)	
		
		(if (= (substr dataline 1 6) "vertex")	;; if the current line is data for a face, process it
			(progn
			
			(setq 
				len  (strlen dataline) 	;;get the length of the data
				pos 1				   	;; get a variable to store the current postion in the data
				xchar nil
				ychar nil
				zchar nil
				x nil
				y nil
				z nil

			)
			
			(repeat len 				;;start a loop to find all the co-ordinates by locating the spaces seperating them
				(if (= (substr dataline pos 1) " ")
					(cond
						((and (and (/= ychar nil) (/= xchar nil)) (= zchar nil)) (setq zchar pos))
						((and (/= xchar nil) (= ychar nil)) (setq ychar pos))				
						((= xchar nil) (setq xchar pos))
					)		
				)					
				(setq pos (+ pos 1)) ;;if the current character isnt a space move to the next character
			)
			
			
			
			(if (and (and (/= xchar nil) (/= ychar nil)) (/= zchar nil))
				(progn
			
			;;set the pt vaiable for x y z
					(setq x (atof (substr dataline xchar (- ychar xchar))))
					(setq y (atof (substr dataline ychar (- zchar ychar))))
					(setq z (atof (substr dataline zchar )))	
					
			;;write to log file for a comparison
					(write-line dataline logfile)
					(setq xyz (strcat "Pt " (rtos counter) " "  (rtos x) " " (rtos y) " " (rtos z) "\n"))
					(write-line xyz logfile)
			
			;; create points from the x y z co-ordinates
					(cond
						((and (and (/= pt2 nil) (/= pt1 nil)) (= pt3 nil)) (setq pt3 (list x y z)))
						((and (/= pt1 nil) (= pt2 nil)) (setq pt2 (list x y z)))				
						((= pt1 nil) (setq pt1 (list x y z)))
					)
				)
			)
			
			
			
			;; use the points to create a 3D face and clear the pt vaiables
			(if (and (and (/= pt2 nil) (/= pt1 nil)) (/= pt3 nil))
				(progn
					;;(princ "\nCreating Face")
					(command "_3dface" pt1 pt2 pt3 "" "")
					;;(command "_line" pt1 pt2 "")
					(setq
						pt1 nil
						pt2 nil	;; clear the pt variables
						pt3 nil							
					)
				)
			)
						
			;;(setq pos 1) ;; reset the position counter for the characters, ready for the next line of data
			(setq counter (+ counter 1))	;; count the number of line that have been processed
			)
			
	
		)
	
	

	(setq dataline (read-line f)) 	;; read the next line of data
	;;(setq counter (+ counter 1))	;; count the number of line that have been processed
	)
	(princ "\n")
	(princ counter)
	(princ " points processed")
	(write-line "EOF" logfile)
	(close f)
	(close logfile)
	(setq counter 0)
	
	;;reset command line output
	(setvar "CMDECHO" 1)
	;; reset previous snap mode
	(setvar "osmode" snapMode)
	(princ)
) ;;EOF















