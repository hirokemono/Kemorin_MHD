!>@file  m_phys_labels.f90
!!       module m_phys_labels
!!
!!@author H. Matsui
!!@date   Programmed on June, 2005
!!
!>@brief Labels of fields
!!
!!@verbatim
!! !!!!!  physical values!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! field names 
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module m_phys_labels
!
      use m_precision
!
      implicit none
!
!>        Field label for time step
      character(len=kchara), parameter :: fhd_t_step = 't_step'
!>        Field label for time
!!         @f$ t @f$
      character(len=kchara), parameter :: fhd_time =   'time'
!
      end module m_phys_labels
