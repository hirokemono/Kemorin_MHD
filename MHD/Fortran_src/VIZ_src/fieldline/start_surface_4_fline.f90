!start_surface_4_fline.f90
!
!      module start_surface_4_fline
!
!      Written by H. Matsui on Aug., 2011
!
!!      subroutine s_start_surface_4_fline(i_fln, node, ele, surf,      &
!!     &          fln_prm, fline_prm, fline_src, fln_tce)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(fieldline_paramter), intent(in) :: fln_prm
!!        type(fieldline_paramters), intent(inout) :: fline_prm
!!        type(all_fieldline_source), intent(inout) :: fline_src
!!        type(each_fieldline_trace), intent(inout) :: fln_tce
!
      module start_surface_4_fline
!
      use m_precision
!
      use calypso_mpi
      use m_constants
      use m_machine_parameter
!
      use t_phys_data
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use t_control_params_4_fline
      use t_source_of_filed_line
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_start_surface_4_fline(i_fln, node, ele, surf,        &
     &          fln_prm, fline_prm, fline_src, fln_src, fln_tce)
!
      use extend_field_line
      use cal_field_on_surf_viz
      use set_fline_start_surface
!
      integer(kind = kint), intent(in) :: i_fln
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
!
      type(fieldline_paramter), intent(in) :: fln_prm
      type(fieldline_paramters), intent(inout) :: fline_prm
      type(all_fieldline_source), intent(inout) :: fline_src
      type(each_fieldline_source), intent(inout) :: fln_src
      type(each_fieldline_trace), intent(inout) :: fln_tce
!
      integer(kind = kint) :: i, ist, ied, inum
      integer(kind = kint) :: ist_line, iele, isf, isurf
      real(kind = kreal) :: vec_surf(3), xi(2)
!
!
      ist_line = fline_prm%istack_each_field_line(i_fln-1)
      do i = 1, fln_src%num_line_local
        inum = i + ist_line
        iele = fline_prm%id_surf_start_fline(1,inum)
        isf =  fline_prm%id_surf_start_fline(2,inum)
        isurf = abs(surf%isf_4_ele(iele,isf))
        fln_src%xx_start_fline(1:3,i) =   surf%x_surf(isurf,1:3)
        xi(1:2) = zero
        call cal_field_on_surf_vector                                   &
     &     (node%numnod, surf%numsurf, surf%nnod_4_surf, surf%ie_surf,  &
     &      isurf, xi, fline_src%vector_nod_fline(1,1,i_fln), vec_surf)
!
        fln_src%flux_start_fline(i)                                     &
     &                     = (vec_surf(1) * surf%vnorm_surf(isurf,1)    &
     &                      + vec_surf(2) * surf%vnorm_surf(isurf,2)    &
     &                      + vec_surf(3) * surf%vnorm_surf(isurf,3))   &
     &                     * dble(surf%isf_4_ele(iele,isf) / isurf)
!
        if(fln_src%flux_start_fline(i) .gt. zero) then
          fline_prm%iflag_outward_flux_fline(inum) = 1
          fln_src%flux_start_fline(i) = -fln_src%flux_start_fline(i)
        end if
      end do
!
      call MPI_AllGather                                                &
     &   (fln_src%num_line_local, ione, CALYPSO_INTEGER,                &
     &    fln_tce%num_current_fline, ione, CALYPSO_INTEGER,             &
     &    CALYPSO_COMM, ierr_MPI)
!
      if(fln_prm%id_fline_direction .eq. iflag_both_trace) then
        fln_tce%num_current_fline(1:nprocs)                             &
     &        = 2 * fln_tce%num_current_fline(1:nprocs)
      end if
!
      fln_tce%istack_current_fline(0) = 0
      do i = 1, nprocs
        fln_tce%istack_current_fline(i)                                 &
     &        = fln_tce%istack_current_fline(i-1)                       &
     &         + fln_tce%num_current_fline(i)
      end do
!
      call set_fline_start_surf(my_rank, i_fln,                         &
     &    node%numnod, ele%numele, surf%numsurf, surf%nnod_4_surf,      &
     &    surf%ie_surf, surf%isf_4_ele, surf%iele_4_surf,               &
     &    fln_prm, fline_prm, fline_src, fln_src, fln_tce)
!
      if(i_debug .gt. iflag_full_msg) then
        write(50+my_rank,*) 'num_current_fline',                        &
     &                   fln_tce%num_current_fline(:)
        write(50+my_rank,*) 'istack_current_fline',                     &
     &                   fln_tce%istack_current_fline(:)
!
        write(50+my_rank,*) 'num_line_local', fln_src%num_line_local
        do i = 1, fln_src%num_line_local
          write(50+my_rank,*) 'id_surf_start_fline', i,                 &
     &                  fline_prm%id_surf_start_fline(1:2,i+ist_line)
          write(50+my_rank,'(a,1p4e16.5)') 'start_point, flux',         &
     &                  fln_src%xx_start_fline(1:3,i),                  &
     &                  fln_src%flux_start_fline(i)
        end do
!
        ist = fln_tce%istack_current_fline(my_rank) + 1
        ied = fln_tce%istack_current_fline(my_rank+1)
        do inum = ist, ied
          write(50+my_rank,*) 'isf_fline_start', inum,                  &
     &                         fln_tce%isf_fline_start(1:3,inum)
          write(50+my_rank,'(a,1p3e16.5)') 'start_point',               &
     &      fln_tce%xx_fline_start(1:3,inum)
        end do
      end if
!
!
      end subroutine s_start_surface_4_fline
!
!  ---------------------------------------------------------------------
!
      end module start_surface_4_fline
