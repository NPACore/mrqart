(defpackage mrqart-5am
  (:use :cl :ciel :5am :mrqart))

(use-package :ciel)
(in-package :mrqart-5am)

(defvar *testdcm*  #P"sim/src/MR.1.3.12.2.1107.5.2.43.167046.2024100414544965356757655" "Path to example dicom file")

(test test-dcm-tr
  (is (equalp
       (mrqart:get-dcm-values *testdcm* (dict :TR "0018,0080"))
       (dict :TR "1300")) ))

(test test-dcm-order-invariant
  "Make sure order or the keys doesn't matter."
  (is
   (equalp
    (mrqart:get-dcm-values *testdcm* (dict :TR "0018,0080" :TE "0018,0081"))
    (mrqart:get-dcm-values *testdcm* (dict :TE "0018,0081" :TR "0018,0080")))))

(test test-dcm-fname
  (is (mrqart::dicom-fname-p *testdcm*))
  (is (mrqart::dicom-fname-p #P"xyx.DCM"))
  (is (mrqart::dicom-fname-p "abc.IMA"))
  (is (not (mrqart::dicom-fname-p "text.log")))
  (is (not (mrqart::dicom-fname-p "MR_dcm_IMA_thing.txt"))))
