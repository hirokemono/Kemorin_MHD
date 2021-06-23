!> @file  m_work_time_4_sleeve_extend.f90
!!      module m_work_time_4_sleeve_extend
!!
!! @author  H. Matsui
!! @date Programmed in June, 2018
!
!> @brief Mark node and element to extend export table
!!
!!@verbatim
!!      subroutine elpsed_label_4_sleeve_ext
!!@endverbatim
!
      module m_work_time_4_sleeve_extend
!
      use m_precision
!
      use m_machine_parameter
      use calypso_mpi
!
      use t_mesh_data
      use t_group_data
      use t_comm_table
      use m_work_time
!
      implicit none
!
      logical, save :: iflag_SLEX_time = .FALSE.
      integer(kind = kint), save :: ist_elapsed_SLEX =   0
      integer(kind = kint), save :: ied_elapsed_SLEX =   0
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine elpsed_label_4_sleeve_ext
!
      integer(kind = kint), parameter :: num_append = 8
!
!
      call append_elapsed_times                                         &
     &   (num_append, ist_elapsed_SLEX, ied_elapsed_SLEX)
!
      elps1%labels(ist_elapsed_SLEX+1)                                  &
     &                    = 'Sleeve extension preparation'
      elps1%labels(ist_elapsed_SLEX+2)                                  &
     &                    = 'Sleeve extension loop'
      elps1%labels(ist_elapsed_SLEX+3)                                  &
     &                    = 'Replace to expanded mesh'
      elps1%labels(ist_elapsed_SLEX+4)                                  &
     &                    = 'Construct sleeve expand list  '
      elps1%labels(ist_elapsed_SLEX+5)                                  &
     &                    = 'Construct extended communication table '
      elps1%labels(ist_elapsed_SLEX+6)                                  &
     &                    = 'Construct extended node data '
      elps1%labels(ist_elapsed_SLEX+7)                                  &
      &                   = 'Construct extended element data '
      elps1%labels(ist_elapsed_SLEX+8)                                  &
     &                    = 'Construct extended goup data  '
!
      iflag_SLEX_time = .TRUE.
!
      end subroutine elpsed_label_4_sleeve_ext
!
!-----------------------------------------------------------------------
!
      end module m_work_time_4_sleeve_extend
