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
      integer(kind = kint), parameter :: num_append = 28
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
      elps1%labels(ist_elapsed_SLEX+ 9)                                 &
     &                    = 'Initialization of extend marking '
      elps1%labels(ist_elapsed_SLEX+10)                                 &
     &                    = 'Extend node marking  '
!
      elps1%labels(ist_elapsed_SLEX+11)                                 &
     &                    = 's_const_extended_neib_domain  '
      elps1%labels(ist_elapsed_SLEX+12)                                 &
     &                    = 'comm_extended_import_nod_ele  '
      elps1%labels(ist_elapsed_SLEX+13)                                 &
     &                    = 'const_trimmed_expand_import  '
      elps1%labels(ist_elapsed_SLEX+14)                                 &
     &                    = 'const_extended_nod_comm_table  '
!
      elps1%labels(ist_elapsed_SLEX+15)                                 &
     &                    = 'set_distance_from_mark_list (init)  '
      elps1%labels(ist_elapsed_SLEX+16)                                 &
     &                    = 'SEND_RECV in init marking  '
      elps1%labels(ist_elapsed_SLEX+17)                                 &
     &                    = 'set_distance_to_mark_list (init)  '
!
      elps1%labels(ist_elapsed_SLEX+18)                                 &
     &                    = 'count_domain_extended_export  '
      elps1%labels(ist_elapsed_SLEX+19)                                 &
     &                    = 'num_items_send_recv in extended_neib '
      elps1%labels(ist_elapsed_SLEX+20)                                 &
     &                    = 'set_domain_to_export_extend  '
      elps1%labels(ist_elapsed_SLEX+21)                                 &
     &                    = 'comm_items_send_recv in extended_neib  '
      elps1%labels(ist_elapsed_SLEX+22)                                 &
     &                    = 'count_extended_neib_import  '
      elps1%labels(ist_elapsed_SLEX+23)                                 &
     &                    = 'count_extended_neib_export  '
      elps1%labels(ist_elapsed_SLEX+24)                                 &
     &                    = 'set_neighbour_domain_by_flag import  '
      elps1%labels(ist_elapsed_SLEX+25)                                 &
     &                    = 'set_neighbour_domain_by_flag export  '
!
      elps1%labels(ist_elapsed_SLEX+26)                                 &
     &                    = 'reset_flags_each_comm_extend (Init)  '
      elps1%labels(ist_elapsed_SLEX+27)                                 &
     &                    = 'set_distance_from_mark_list (Init actual)'
      elps1%labels(ist_elapsed_SLEX+28)                                 &
     &                    = 'dealloc_mark_for_each_comm (Init)  '
!
      iflag_SLEX_time = .TRUE.
!
      end subroutine elpsed_label_4_sleeve_ext
!
!-----------------------------------------------------------------------
!
      end module m_work_time_4_sleeve_extend
