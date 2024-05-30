!>@file   trace_particle.f90
!!@brief  module trace_particle
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2011
!
!> @brief Routines to construct field lines
!!
!!@verbatim
!!      subroutine s_trace_particle(dt_init, node, ele, surf,           &
!!     &          isf_4_ele_dbl, iele_4_surf_dbl,                       &
!!     &          nod_fld, fln_prm, fln_tce, fln_bcast, v_prev)
!!        real(kind = kreal), intent(in) :: dt_init
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(phys_data), intent(in) :: nod_fld
!!        type(fieldline_paramter), intent(in) :: fln_prm
!!        type(each_fieldline_trace), intent(inout) :: fln_tce
!!        type(broadcast_trace_data), intent(inout) :: fln_bcast
!!@endverbatim
!
      module trace_particle
!
      use m_precision
!
      use calypso_mpi
      use m_constants
      use m_machine_parameter
      use m_geometry_constants
      use t_geometry_data
      use t_surface_data
      use t_comm_table
      use t_para_double_numbering
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_trace_particle(dt_init, node, ele, surf,             &
     &          isf_4_ele_dbl, iele_4_surf_dbl,                         &
     &          nod_fld, fln_prm, fln_tce, fln_bcast, v_prev)
!
      use t_control_params_4_fline
      use t_comm_table
      use t_next_node_ele_4_node
      use t_phys_data
      use t_source_of_filed_line
      use t_broadcast_trace_data
      use calypso_mpi_real
      use calypso_mpi_int
      use transfer_to_long_integers
      use trace_particle_in_element
!
      real(kind = kreal), intent(in) :: dt_init
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(phys_data), intent(in) :: nod_fld
      integer(kind = kint), intent(in)                                  &
     &               :: isf_4_ele_dbl(ele%numele,nsurf_4_ele,2)
      integer(kind = kint), intent(in)                                  &
     &               :: iele_4_surf_dbl(surf%numsurf,2,3)
!
      type(fieldline_paramter), intent(in) :: fln_prm
      type(each_fieldline_trace), intent(inout) :: fln_tce
      type(broadcast_trace_data), intent(inout) :: fln_bcast
      real(kind = kreal), intent(inout) :: v_prev(nod_fld%n_point,3)
!
      real(kind = kreal) :: dt
      integer(kind = kint) :: iflag_comm
      integer(kind = kint) :: i, ist, ied, ip, nline, inum, num
      integer(kind = kint_gl) :: num64
      integer :: src_rank
!
!
      if(i_debug .gt. iflag_full_msg) then
        write(my_rank+50,*)                                             &
     &         'num_current_fline', fln_tce%num_current_fline(:)
        write(my_rank+50,*)                                             &
     &         'istack_current_fline', fln_tce%istack_current_fline(:)
        ist = fln_tce%istack_current_fline(my_rank) + 1
        ied = fln_tce%istack_current_fline(my_rank+1)
        write(my_rank+50,*) 'isf_fline_start(1:2,inum)'
        do inum = ist, ied
          write(my_rank+50,*) inum, fln_tce%isf_fline_start(1:2,inum)
        end do
      end if
      call calypso_MPI_barrier
!
      iflag_comm = 0
      dt = dt_init
      do
        write(*,*) 'fln_tce%istack_current_fline', my_rank,             &
     &            fln_tce%istack_current_fline(my_rank:my_rank+1),      &
     &            fln_tce%num_current_fline(my_rank+1)
        do inum = 1, fln_tce%num_current_fline(my_rank+1)
          call s_trace_particle_in_element                              &
     &       (dt, node, surf, nod_fld, v_prev,                          &
     &        fln_prm%fline_fields, fln_prm%iphys_4_fline,              &
     &        fln_tce%isf_fline_start(1,inum),                          &
     &        fln_tce%xx_fline_start(1,inum),                           &
     &        fln_tce%v_fline_start(1,inum),                            &
     &        fln_tce%c_fline_start(1,inum),                            &
     &        iflag_comm)
          write(50+my_rank,*) 'extension end for ', inum, iflag_comm
!
          call set_fline_start_2_bcast(iflag_comm, inum, ele, surf,     &
     &        isf_4_ele_dbl, iele_4_surf_dbl, fln_tce, fln_bcast)
        end do
!
        call s_broadcast_trace_data(fln_tce, fln_bcast, nline)

        if(i_debug .gt. 0) then
          write(my_rank+50,*) 'istack_current_fline',                   &
     &                       fln_tce%istack_current_fline(:)
!
          write(my_rank+50,*) 'number of lines: ', nline
          write(*,*) 'number of lines: ', my_rank, nline
        end if
       if(nline .le. 0) exit
      end do
!
      end subroutine s_trace_particle
!
!  ---------------------------------------------------------------------
!
      end module trace_particle
