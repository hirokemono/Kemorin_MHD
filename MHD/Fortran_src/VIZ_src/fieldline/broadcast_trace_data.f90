!>@file   broadcast_trace_data.f90
!!@brief  module broadcast_trace_data
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2011
!
!> @brief Routines to construct field lines
!!
!!@verbatim
!!      subroutine s_broadcast_trace_data(fln_prm, fln_tce,             &
!!     &                                  nline_global)
!!        type(fieldline_paramter), intent(in) :: fln_prm
!!        type(each_fieldline_trace), intent(inout) :: fln_tce
!!        integer(kind = kint), intent(inout) :: nline_global
!!      subroutine set_fline_start_2_bcast(iflag_comm, i, ele, surf,    &
!!     &          isf_4_ele_dbl, iele_4_surf_dbl, viz_fields, fln_tce)
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(ctl_params_viz_fields), intent(in) :: viz_fields
!!        integer(kind = kint), intent(in)                              &
!!     &               :: isf_4_ele_dbl(ele%numele,nsurf_4_ele,2)
!!        integer(kind = kint), intent(in)                              &
!!     &               :: iele_4_surf_dbl(surf%numsurf,2,3)
!!        type(each_fieldline_trace), intent(inout) :: fln_tce
!!@endverbatim
!
      module broadcast_trace_data
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
      private :: set_fline_start_from_neib
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_broadcast_trace_data(fln_prm, fln_tce,               &
     &                                  nline_global)
!
      use t_control_params_4_fline
      use t_source_of_filed_line
      use calypso_mpi_real
      use calypso_mpi_int
      use transfer_to_long_integers
!
      type(fieldline_paramter), intent(in) :: fln_prm
      type(each_fieldline_trace), intent(inout) :: fln_tce
      integer(kind = kint), intent(inout) :: nline_global
!
      integer(kind = kint) :: ist, ip
      integer(kind = kint_gl) :: num64
      integer :: src_rank
!
!
        do ip = 1, nprocs
          src_rank = int(ip - 1)
          ist = fln_tce%istack_current_fline(ip-1)
          num64 = fln_tce%num_current_fline(ip)
          if(num64 .le. 0) cycle
            call calypso_mpi_bcast_int                                  &
     &         (fln_tce%id_fline_export(1,ist+1),                       &
     &          (num64*fln_tce%nitem_export), src_rank)
            call calypso_mpi_bcast_real(fln_tce%fline_export(1,ist+1),  &
     &          (num64*fln_tce%ncomp_export), src_rank)
        end do
!
        call set_fline_start_from_neib(fln_prm%fline_fields, fln_tce)
!
        nline_global = fln_tce%istack_current_fline(nprocs)             &
     &               - fln_tce%istack_current_fline(0)
!
      end subroutine s_broadcast_trace_data
!
!  ---------------------------------------------------------------------
!
      subroutine set_fline_start_2_bcast(iflag_comm, i, ele, surf,      &
     &          isf_4_ele_dbl, iele_4_surf_dbl, viz_fields, fln_tce)
!
      use t_source_of_filed_line
!
      integer(kind = kint), intent(in) :: iflag_comm, i
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(ctl_params_viz_fields), intent(in) :: viz_fields
      integer(kind = kint), intent(in)                                  &
     &               :: isf_4_ele_dbl(ele%numele,nsurf_4_ele,2)
      integer(kind = kint), intent(in)                                  &
     &               :: iele_4_surf_dbl(surf%numsurf,2,3)
!
      type(each_fieldline_trace), intent(inout) :: fln_tce
!
      integer(kind = kint) :: iele, isf, ist
      integer(kind = kint) :: isurf
!
!
      ist = fln_tce%istack_current_fline(my_rank)
      if(iflag_comm .eq. ione) then
        iele = fln_tce%isf_fline_start(1,i)
        isf =  fln_tce%isf_fline_start(2,i)
        isurf = abs(surf%isf_4_ele(iele,isf))
!
        fln_tce%id_fline_export(1,i+ist) = fln_tce%iline_original(i)
        fln_tce%id_fline_export(2,i+ist) = fln_tce%iflag_fline(i)
        fln_tce%id_fline_export(3,i+ist) = fln_tce%icount_fline(i)
!
        if(isf_4_ele_dbl(iele,isf,2) .lt. 0) then
          fln_tce%id_fline_export(4:5,i+ist)                            &
     &               = iele_4_surf_dbl(isurf, 1, 2:3)
        else
          fln_tce%id_fline_export(4:5,i+ist)                            &
     &               = iele_4_surf_dbl(isurf, 2, 2:3)
        end if
        fln_tce%id_fline_export(6,i+ist) = isf_4_ele_dbl(iele,isf,1)
!
        fln_tce%fline_export(1:4,i+ist) = fln_tce%xx_fline_start(1:4,i)
        fln_tce%fline_export(5:8,i+ist) = fln_tce%v_fline_start(1:4,i)
        fln_tce%fline_export(9,i+ist) =   fln_tce%trace_length(i)
        fln_tce%fline_export(9+1:9+viz_fields%ntot_color_comp,i+ist)    &
    &         = fln_tce%c_fline_start(1:viz_fields%ntot_color_comp,i)
      else
        fln_tce%id_fline_export(1:5,i+ist) = izero
        fln_tce%id_fline_export(6,i+ist) =  -ione
        fln_tce%fline_export(1:fln_tce%ncomp_export,i+ist) = zero
      end if
!
      end subroutine set_fline_start_2_bcast
!
!  ---------------------------------------------------------------------
!
      subroutine set_fline_start_from_neib(viz_fields, fln_tce)
!
      use calypso_mpi_int
      use t_source_of_filed_line
!
      type(ctl_params_viz_fields), intent(in) :: viz_fields
      type(each_fieldline_trace), intent(inout) :: fln_tce
!
      integer(kind = kint) :: ied_lin, i, icou, ip
!
!
!
      ied_lin = fln_tce%istack_current_fline(nprocs)
      icou = 0
      do i = 1, ied_lin
        if(fln_tce%id_fline_export(6,i) .eq. my_rank) then
          icou = icou + 1
        end if
      end do
!
      call calypso_mpi_allgather_one_int                                &
     &   (icou, fln_tce%num_current_fline)
!
      do ip = 1, nprocs
        fln_tce%istack_current_fline(ip)                                &
     &                   = fln_tce%istack_current_fline(ip-1)           &
     &                    + fln_tce%num_current_fline(ip)
      end do
!
      icou = 0
      do i = 1, ied_lin
        if(fln_tce%id_fline_export(6,i) .eq. my_rank) then
          icou = icou + 1
          fln_tce%iline_original(icou) = fln_tce%id_fline_export(1,i)
          fln_tce%iflag_fline(icou) =  fln_tce%id_fline_export(2,i)
          fln_tce%icount_fline(icou) = fln_tce%id_fline_export(3,i)
          fln_tce%isf_fline_start(1:2,icou)                             &
     &         = fln_tce%id_fline_export(4:5,i)
!
          fln_tce%xx_fline_start(1:4,icou)                              &
     &         = fln_tce%fline_export(1:4,i)
          fln_tce%v_fline_start(1:4,icou) = fln_tce%fline_export(5:8,i)
          fln_tce%trace_length(icou) = fln_tce%fline_export(9,i)
          fln_tce%c_fline_start(1:viz_fields%ntot_color_comp,icou)      &
     &       = fln_tce%fline_export(9+1:9+viz_fields%ntot_color_comp,i)
        end if
      end do
!
      end subroutine set_fline_start_from_neib
!
!  ---------------------------------------------------------------------
!
      end module broadcast_trace_data

