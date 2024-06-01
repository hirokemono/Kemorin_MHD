!>@file   t_broadcast_trace_data.f90
!!@brief  module t_broadcast_trace_data
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2011
!
!> @brief Routines to construct field lines
!!
!!@verbatim
!!      subroutine alloc_broadcast_trace_data(num_each_field_line,      &
!!     &                                      viz_fields, fln_bcast)
!!      subroutine dealloc_broadcast_trace_data(fln_bcast)
!!        type(ctl_params_viz_fields), intent(in) :: viz_fields
!!        type(broadcast_trace_data), intent(inout) :: fln_bcast
!!
!!      subroutine s_broadcast_trace_data                               &
!!     &         (ele, surf, isf_4_ele_dbl, iele_4_surf_dbl,            &
!!     &          fln_tce, fln_bcast, nline_global)
!!        type(each_fieldline_trace), intent(inout) :: fln_tce
!!        type(broadcast_trace_data), intent(inout) :: fln_bcast
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        integer(kind = kint), intent(in)                              &
!!     &               :: isf_4_ele_dbl(ele%numele,nsurf_4_ele,2)
!!        integer(kind = kint), intent(in)                              &
!!     &               :: iele_4_surf_dbl(surf%numsurf,2,3)
!!        type(each_fieldline_trace), intent(inout) :: fln_tce
!!        type(broadcast_trace_data), intent(inout) :: fln_bcast
!!        integer(kind = kint), intent(inout) :: nline_global
!!@endverbatim
!
      module t_broadcast_trace_data
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
      integer(kind= kint), parameter, private :: nitem_bcast = 6
!
      type broadcast_trace_data
        integer(kind= kint) :: ncomp_bcast
        integer(kind= kint), allocatable :: id_fline_export(:,:)
        real(kind = kreal), allocatable ::  fline_export(:,:)
      end type broadcast_trace_data

      private :: set_fline_start_2_bcast, set_fline_start_from_neib
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_broadcast_trace_data(num_each_field_line,        &
     &                                      viz_fields, fln_bcast)
!
      use t_ctl_params_viz_fields
!
      integer(kind = kint), intent(in) :: num_each_field_line
      type(ctl_params_viz_fields), intent(in) :: viz_fields
      type(broadcast_trace_data), intent(inout) :: fln_bcast
!
      integer(kind = kint) :: num
!
!
      num = 2 * num_each_field_line
      fln_bcast%ncomp_bcast = 9 + viz_fields%ntot_color_comp
      allocate(fln_bcast%id_fline_export(nitem_bcast,num))
      allocate(fln_bcast%fline_export(fln_bcast%ncomp_bcast,num))
      fln_bcast%id_fline_export = 0
      fln_bcast%fline_export = 0.0d0
!
      end subroutine alloc_broadcast_trace_data
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_broadcast_trace_data(fln_bcast)
!
      type(broadcast_trace_data), intent(inout) :: fln_bcast
!
      deallocate(fln_bcast%id_fline_export)
      deallocate(fln_bcast%fline_export)
!
      end subroutine dealloc_broadcast_trace_data
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine s_broadcast_trace_data                                 &
     &         (ele, surf, isf_4_ele_dbl, iele_4_surf_dbl,              &
     &          fln_tce, fln_bcast, nline_global)
!
      use t_source_of_filed_line
      use calypso_mpi_real
      use calypso_mpi_int
      use transfer_to_long_integers
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      integer(kind = kint), intent(in)                                  &
     &               :: isf_4_ele_dbl(ele%numele,nsurf_4_ele,2)
      integer(kind = kint), intent(in)                                  &
     &               :: iele_4_surf_dbl(surf%numsurf,2,3)
!
      type(each_fieldline_trace), intent(inout) :: fln_tce
      type(broadcast_trace_data), intent(inout) :: fln_bcast
      integer(kind = kint), intent(inout) :: nline_global
!
      integer(kind = kint) :: ist, ip, inum
      integer(kind = kint_gl) :: num64
      integer :: src_rank
!
!
      do inum = 1, fln_tce%num_current_fline
        call set_fline_start_2_bcast(inum, ele, surf,                   &
     &      isf_4_ele_dbl, iele_4_surf_dbl, fln_tce, fln_bcast)
      end do
!
        do ip = 1, nprocs
          src_rank = int(ip - 1)
          ist = fln_tce%istack_current_fline(ip-1)
          num64 = fln_tce%istack_current_fline(ip) - ist
          if(num64 .le. 0) cycle
            call calypso_mpi_bcast_int                                  &
     &         (fln_bcast%id_fline_export(1,ist+1),                     &
     &          (num64*nitem_bcast), src_rank)
            call calypso_mpi_bcast_real                                 &
     &         (fln_bcast%fline_export(1,ist+1),                        &
     &          (num64*fln_bcast%ncomp_bcast), src_rank)
        end do
!
        call set_fline_start_from_neib(fln_bcast, fln_tce)
!
        nline_global = fln_tce%istack_current_fline(nprocs)             &
     &               - fln_tce%istack_current_fline(0)
!
      end subroutine s_broadcast_trace_data
!
!  ---------------------------------------------------------------------
!
      subroutine set_fline_start_2_bcast(i, ele, surf,                  &
     &          isf_4_ele_dbl, iele_4_surf_dbl, fln_tce, fln_bcast)
!
      use t_source_of_filed_line
!
      integer(kind = kint), intent(in) :: i
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      integer(kind = kint), intent(in)                                  &
     &               :: isf_4_ele_dbl(ele%numele,nsurf_4_ele,2)
      integer(kind = kint), intent(in)                                  &
     &               :: iele_4_surf_dbl(surf%numsurf,2,3)
      type(each_fieldline_trace), intent(in) :: fln_tce
!
      type(broadcast_trace_data), intent(inout) :: fln_bcast
!
      integer(kind = kint) :: iele, isf, ist
      integer(kind = kint) :: isurf
!
!
      ist = fln_tce%istack_current_fline(my_rank)
      if(fln_tce%iflag_comm_start(i) .eq. ione) then
        iele = fln_tce%isf_fline_start(1,i)
        isf =  fln_tce%isf_fline_start(2,i)
        isurf = abs(surf%isf_4_ele(iele,isf))
!
        fln_bcast%id_fline_export(1,i+ist) = fln_tce%iline_original(i)
        fln_bcast%id_fline_export(2,i+ist) = fln_tce%iflag_direction(i)
        fln_bcast%id_fline_export(3,i+ist) = fln_tce%icount_fline(i)
!
        if(isf_4_ele_dbl(iele,isf,2) .lt. 0) then
          fln_bcast%id_fline_export(4:5,i+ist)                          &
     &               = iele_4_surf_dbl(isurf, 1, 2:3)
        else
          fln_bcast%id_fline_export(4:5,i+ist)                          &
     &               = iele_4_surf_dbl(isurf, 2, 2:3)
        end if
        fln_bcast%id_fline_export(6,i+ist) = isf_4_ele_dbl(iele,isf,1)
!
        fln_bcast%fline_export(1:4,i+ist)                               &
     &               = fln_tce%xx_fline_start(1:4,i)
        fln_bcast%fline_export(5:8,i+ist)                               &
     &               = fln_tce%v_fline_start(1:4,i)
        fln_bcast%fline_export(9,i+ist) =   fln_tce%trace_length(i)
        fln_bcast%fline_export(9+1:fln_bcast%ncomp_bcast,i+ist)         &
    &         = fln_tce%c_fline_start(1:fln_bcast%ncomp_bcast-9,i)
      else
        fln_bcast%id_fline_export(1:5,i+ist) = izero
        fln_bcast%id_fline_export(6,i+ist) =  -ione
        fln_bcast%fline_export(1:fln_bcast%ncomp_bcast,i+ist) = zero
      end if
!
      end subroutine set_fline_start_2_bcast
!
!  ---------------------------------------------------------------------
!
      subroutine set_fline_start_from_neib(fln_bcast, fln_tce)
!
      use calypso_mpi_int
      use t_source_of_filed_line
!
      type(broadcast_trace_data), intent(in) :: fln_bcast
      type(each_fieldline_trace), intent(inout) :: fln_tce
!
      integer(kind = kint) :: ied_lin, i, icou, ip
!
!
!
      ied_lin = fln_tce%istack_current_fline(nprocs)
      icou = 0
      do i = 1, ied_lin
        if(fln_bcast%id_fline_export(6,i) .eq. my_rank) then
          icou = icou + 1
        end if
      end do
      fln_tce%num_current_fline = icou
!
      fln_tce%istack_current_fline(0) = 0
      call calypso_mpi_allgather_one_int(fln_tce%num_current_fline,     &
     &                                 fln_tce%istack_current_fline(1))
!
      do ip = 1, nprocs
        fln_tce%istack_current_fline(ip)                                &
     &                   = fln_tce%istack_current_fline(ip-1)           &
     &                    + fln_tce%istack_current_fline(ip)
      end do
!
      icou = 0
      do i = 1, ied_lin
        if(fln_bcast%id_fline_export(6,i) .eq. my_rank) then
          icou = icou + 1
          fln_tce%iline_original(icou) = fln_bcast%id_fline_export(1,i)
          fln_tce%iflag_direction(icou)                                 &
     &                                 = fln_bcast%id_fline_export(2,i)
          fln_tce%icount_fline(icou) =   fln_bcast%id_fline_export(3,i)
          fln_tce%isf_fline_start(1:2,icou)                             &
     &                               = fln_bcast%id_fline_export(4:5,i)
!
          fln_tce%xx_fline_start(1:4,icou)                              &
     &                               = fln_bcast%fline_export(1:4,i)
          fln_tce%v_fline_start(1:4,icou)                               &
     &                               = fln_bcast%fline_export(5:8,i)
          fln_tce%trace_length(icou) = fln_bcast%fline_export(9,i)
          fln_tce%c_fline_start(1:fln_bcast%ncomp_bcast-9,icou)         &
     &            = fln_bcast%fline_export(9+1:fln_bcast%ncomp_bcast,i)
        end if
      end do
!
      end subroutine set_fline_start_from_neib
!
!  ---------------------------------------------------------------------
!
      end module t_broadcast_trace_data

