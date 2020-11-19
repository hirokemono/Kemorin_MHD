!>@file  m_interpolation_data_labels.f90
!!       module m_interpolation_data_labels
!!
!!@author H. Matsui
!!@date        programmed by H.Matsui in July, 2007
!
!> @brief Comments for intepolation data
!!
!!@verbatim
!!      character(len=ilen_itp_import_pe) function hd_itp_import_pe()
!!      character(len=ilen_itp_import_item)                             &
!!     &            function hd_itp_import_item()
!!
!!      character(len=ilen_itp_export_pe) function hd_itp_export_pe()
!!      character(len=ilen_itp_export_item)                             &
!!     &            function hd_itp_export_item()
!!      character(len=ilen_itp_export_coef)                             &
!!     &            function hd_itp_export_coef()
!!
!!      character(len=ilen_itp_dest_coef) function hd_itp_dest_coef()
!!@endverbatim
!
      module m_interpolation_data_labels
!
      use m_precision
!
      integer(kind = kint), parameter                                   &
     &          :: ilen_itp_import_pe = 1+12+29+23+1+5
      integer(kind = kint), parameter                                   &
     &          :: ilen_itp_import_item = 1+26+19+1+4
!
      integer(kind = kint), parameter                                   &
     &          :: ilen_itp_export_pe = 1+12+29+23+1+5
      integer(kind = kint), parameter                                   &
     &          :: ilen_itp_export_item = 1+26+19+1+4
      integer(kind = kint), parameter                                   &
     &          :: ilen_itp_export_coef = 1+30+28+37+24+23+1+7
!
      integer(kind = kint), parameter                                   &
     &          :: ilen_itp_dest_coef = 1+30+25+28+24+23+1+7
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      character(len=ilen_itp_import_pe) function hd_itp_import_pe()
!
      hd_itp_import_pe                                                  &
     &           = '!' // char(10)                                      &
     &          // '!  domain ID' // char(10)                           &
     &          // '!  number of domain to import' // char(10)          &
     &          // '!  domain IDs to import' // char(10)                &
     &          // '!' // char(10)
!
      end function hd_itp_import_pe
!
!------------------------------------------------------------------
!
      character(len=ilen_itp_import_item)                               &
     &            function hd_itp_import_item()
!
      hd_itp_import_item                                                &
     &           = '!' // char(10)                                      &
     &          // '!  stack of node to import' // char(10)             &
     &          // '!  imported node ID' // char(10)                    &
     &          // '!' // char(10)
!
      end function hd_itp_import_item
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      character(len=ilen_itp_export_pe) function hd_itp_export_pe()
!
      hd_itp_export_pe                                                  &
     &           = '!' // char(10)                                      &
     &          // '!  domain ID' // char(10)                           &
     &          // '!  number of domain to export' // char(10)          &
     &          // '!  domain IDs to export' // char(10)                &
     &          // '!' // char(10)
!
      end function hd_itp_export_pe
!
!------------------------------------------------------------------
!
      character(len=ilen_itp_export_item)                               &
     &            function hd_itp_export_item()
!
      hd_itp_export_item                                                &
     &           = '!' // char(10)                                      &
     &          // '!  stack of node to export' // char(10)             &
     &          // '!  exported node ID' // char(10)                    &
     &          // '!' // char(10)
!
      end function hd_itp_export_item
!
!------------------------------------------------------------------
!
      character(len=ilen_itp_export_coef)                               &
     &            function hd_itp_export_coef()
!
      hd_itp_export_coef                                                &
     &           = '!' // char(10)                                      &
     &          // '!  stack by interpolation type' // char(10)         &
     &          // '!  destinate global node ID,' // char(10)           &
     &          // '!  local element ID for interpolation' // char(10)  &
     &          // '!  interpolation type ID' // char(10)               &
     &          // '!  generalized position' // char(10)                &
     &          // '!' // char(10)
!
      end function hd_itp_export_coef
!
!------------------------------------------------------------------
!
      character(len=ilen_itp_dest_coef) function hd_itp_dest_coef()
!
      hd_itp_dest_coef                                                  &
     &           = '!' // char(10)                                      &
     &          // '!  stack by interpolation type' // char(10)         &
     &          // '!  target global node ID,' // char(10)              &
     &          // '!  belonged local element ID' // char(10)           &
     &          // '!  interpolation type ID' // char(10)               &
     &          // '!  generalized position' // char(10)                &
     &          // '!' // char(10)
!
      end function hd_itp_dest_coef
!
!------------------------------------------------------------------
!
      end module m_interpolation_data_labels
