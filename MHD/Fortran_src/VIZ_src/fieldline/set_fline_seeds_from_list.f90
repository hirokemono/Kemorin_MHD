!>@file   set_fline_seeds_from_list.f90
!!@brief  module set_fline_seeds_from_list
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2011
!
!> @brief Main routine for field line module
!!
!!@verbatim
!!      subroutine const_FLINE_seed_from_list(node, ele, nod_fld,       &
!!     &                                      fln_prm, fln_src, fln_tce)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(phys_data), intent(in) :: nod_fld
!!        type(fieldline_paramter), intent(in) :: fln_prm
!!        type(each_fieldline_source), intent(in) :: fln_src
!!        type(each_fieldline_trace), intent(inout) :: fln_tce
!!@endverbatim
!
      module set_fline_seeds_from_list
!
      use m_precision
      use calypso_mpi
!
      use m_machine_parameter
      use m_geometry_constants
      use t_geometry_data
      use t_phys_data
      use t_control_params_4_fline
      use t_source_of_filed_line
      use t_tracing_data
!
      implicit none
!
      real(kind = kreal), parameter, private ::   error_level = 1.0d-9
!
      private :: count_FLINE_seed_from_list
      private :: set_FLINE_seed_field_from_list
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine const_FLINE_seed_from_list(node, ele, nod_fld,         &
     &                                      fln_prm, fln_src, fln_tce)
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
      type(fieldline_paramter), intent(in) :: fln_prm
      type(each_fieldline_source), intent(in) :: fln_src
!
      type(each_fieldline_trace), intent(inout) :: fln_tce
!
!
      call count_FLINE_seed_from_list(fln_src%num_line_local,           &
     &                                fln_prm, fln_tce)
      call set_FLINE_seed_field_from_list(node, ele, nod_fld, fln_prm,  &
     &                                    fln_src, fln_tce)
      call check_line_start_fline(fln_tce)
!
      end subroutine const_FLINE_seed_from_list
!
!  ---------------------------------------------------------------------
!
      subroutine count_FLINE_seed_from_list(num_line_local,             &
     &                                      fln_prm, fln_tce)
!
      use calypso_mpi_int
!
      integer(kind = kint), intent(in) :: num_line_local
      type(fieldline_paramter), intent(in) :: fln_prm
      type(each_fieldline_trace), intent(inout) :: fln_tce
!
      integer(kind = kint) :: i
!
!
      fln_tce%num_current_fline = num_line_local
      if(fln_prm%id_fline_direction .eq. iflag_both_trace) then
        fln_tce%num_current_fline = 2 * fln_tce%num_current_fline
      end if
      call resize_line_start_fline(fln_tce%num_current_fline,           &
     &                             fln_prm%fline_fields, fln_tce)
!
      fln_tce%istack_current_fline(0) = 0
      call calypso_mpi_allgather_one_int(fln_tce%num_current_fline,     &
     &                                 fln_tce%istack_current_fline(1))
      do i = 1, nprocs
        fln_tce%istack_current_fline(i)                                 &
     &     = fln_tce%istack_current_fline(i-1)                          &
     &      + fln_tce%istack_current_fline(i)
      end do
!
      end subroutine count_FLINE_seed_from_list
!
!  ---------------------------------------------------------------------
!
      subroutine set_FLINE_seed_field_from_list                         &
     &         (node, ele, nod_fld, fln_prm, fln_src, fln_tce)
!
      use sel_interpolate_scalar
      use extend_field_line
      use trace_in_element
      use tracer_field_interpolate
      use field_at_each_seed_point
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
      type(fieldline_paramter), intent(in) :: fln_prm
      type(each_fieldline_source), intent(in) :: fln_src
!
      type(each_fieldline_trace), intent(inout) :: fln_tce
!
      integer(kind = kint) :: icou, inum
!
!
      icou = 0
      do inum = 1, fln_prm%num_each_field_line
          if(fln_src%ip_surf_start_fline(inum) .ne. my_rank) cycle
          icou = icou + 1
!
          call cal_each_seed_velocity_in_ele(ele, nod_fld%n_point,      &
     &        nod_fld%d_fld(1,fln_prm%iphys_4_fline),                   &
     &        fln_src%iele_surf_start_fline(inum),                      &
     &        fln_src%xi_surf_start_fline(1,inum),                      &
     &        fln_tce%v_fline_start(1,icou))
          call cal_fields_in_element                                    &
     &       (fln_src%iele_surf_start_fline(inum),                      &
     &        fln_src%xi_surf_start_fline(1,inum),                      &
     &        fln_prm%xx_surf_start_fline(1,inum),                      &
     &        ele, nod_fld, fln_prm%fline_fields,                       &
     &        fln_tce%c_fline_start(1,icou))
!
!
          fln_tce%isf_dbl_start(1,icou) = my_rank
          fln_tce%isf_dbl_start(2,icou)                                 &
     &      = fln_src%iele_surf_start_fline(inum)
          fln_tce%isf_dbl_start(3,icou)                                 &
     &            = surface_mode_in_each_ele(error_level,               &
     &             fln_src%xi_surf_start_fline(1,inum))
!
          fln_tce%iline_original(icou) = inum
          fln_tce%xx_fline_start(1:3,icou)                              &
     &         = fln_prm%xx_surf_start_fline(1:3,inum)
          fln_tce%xx_fline_start(4,icou) = one
          fln_tce%trace_length(icou) = 0.0d0
          fln_tce%icount_fline(icou) = 0
          
          if     (fln_prm%id_fline_direction                            &
     &                  .eq. iflag_forward_trace) then
           fln_tce%iflag_direction(icou) = 1
          else if(fln_prm%id_fline_direction                            &
     &                  .eq. iflag_backward_trace) then

            fln_tce%iflag_direction(icou) = -1
          else
            fln_tce%iflag_direction(icou) = 1
!
            icou = icou + 1
            fln_tce%iflag_direction(icou) = -1
            fln_tce%isf_dbl_start(1,icou) = my_rank
            fln_tce%isf_dbl_start(2,icou)                               &
     &            = fln_src%iele_surf_start_fline(inum)
            fln_tce%isf_dbl_start(3,icou)                               &
     &            = surface_mode_in_each_ele(error_level,               &
     &             fln_src%xi_surf_start_fline(1,inum))
!
            fln_tce%trace_length(icou) = 0.0d0
            fln_tce%icount_fline(icou) = 0
            call copy_global_start_fline(icou, (icou-1),                &
     &                                   fln_prm%fline_fields, fln_tce)

          end if
        end do
!
      end subroutine set_FLINE_seed_field_from_list
!
!  ---------------------------------------------------------------------
!
      end module set_fline_seeds_from_list
