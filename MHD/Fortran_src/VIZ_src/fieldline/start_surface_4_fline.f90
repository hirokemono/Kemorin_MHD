!start_surface_4_fline.f90
!
!      module start_surface_4_fline
!
!      Written by H. Matsui on Aug., 2011
!
!!      subroutine s_start_surface_4_fline(i_fln, node, ele, surf,      &
!!     &          fln_prm, fline_prm, fline_src, fline_tce)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(fieldline_paramter), intent(in) :: fln_prm
!!        type(fieldline_paramters), intent(inout) :: fline_prm
!!        type(fieldline_source), intent(inout) :: fline_src
!!        type(fieldline_trace), intent(inout) :: fline_tce
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
     &          fln_prm, fline_prm, fline_src, fline_tce)
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
      type(fieldline_source), intent(inout) :: fline_src
      type(fieldline_trace), intent(inout) :: fline_tce
!
      integer(kind = kint) :: i, ist, ied, inum
      integer(kind = kint) :: ist_line, iele, isf, isurf
      real(kind = kreal) :: vec_surf(3), xi(2)
!
!
      ist_line = fline_prm%istack_each_field_line(i_fln-1)
      do i = 1, fline_src%num_line_local(i_fln)
        inum = i + ist_line
        iele = fline_prm%id_surf_start_fline(1,inum)
        isf =  fline_prm%id_surf_start_fline(2,inum)
        isurf = abs(surf%isf_4_ele(iele,isf))
        fline_src%xx_start_fline(1:3,inum) =   surf%x_surf(isurf,1:3)
        xi(1:2) = zero
        call cal_field_on_surf_vector                                   &
     &     (node%numnod, surf%numsurf, surf%nnod_4_surf, surf%ie_surf,  &
     &      isurf, xi, fline_src%vector_nod_fline(1,1,i_fln), vec_surf)
!
        fline_src%flux_start_fline(inum)                                &
     &                     = (vec_surf(1) * surf%vnorm_surf(isurf,1)    &
     &                      + vec_surf(2) * surf%vnorm_surf(isurf,2)    &
     &                      + vec_surf(3) * surf%vnorm_surf(isurf,3))   &
     &                     * dble(surf%isf_4_ele(iele,isf) / isurf)
!
        if(fline_src%flux_start_fline(inum) .gt. zero) then
          fline_prm%iflag_outward_flux_fline(inum) = 1
          fline_src%flux_start_fline(inum)                              &
     &                     = -fline_src%flux_start_fline(inum)
        end if
      end do
!
      call MPI_AllGather                                                &
     &   (fline_src%num_line_local(i_fln), ione, CALYPSO_INTEGER,       &
     &    fline_tce%num_all_fline(1,i_fln), ione, CALYPSO_INTEGER,      &
     &    CALYPSO_COMM, ierr_MPI)
!
      if(fln_prm%id_fline_direction .eq. iflag_both_trace) then
        fline_tce%num_all_fline(1:nprocs,i_fln)                         &
     &        = 2 * fline_tce%num_all_fline(1:nprocs,i_fln)
      end if
!
      fline_tce%istack_all_fline(0,i_fln) = fline_tce%ntot_gl_fline
      do i = 1, nprocs
        fline_tce%istack_all_fline(i,i_fln)                             &
     &        = fline_tce%istack_all_fline(i-1,i_fln)                   &
     &         + fline_tce%num_all_fline(i,i_fln)
      end do
      fline_tce%ntot_gl_fline                                           &
     &        = fline_tce%istack_all_fline(nprocs,i_fln)
!
      call set_fline_start_surf(my_rank, i_fln,                         &
     &    node%numnod, ele%numele, surf%numsurf, surf%nnod_4_surf,      &
     &    surf%ie_surf, surf%isf_4_ele, surf%iele_4_surf,               &
     &    fln_prm, fline_prm, fline_src, fline_tce)
!
      if(i_debug .gt. iflag_full_msg) then
        write(50+my_rank,*) 'ntot_gl_fline', fline_tce%ntot_gl_fline
        write(50+my_rank,*) 'ntot_gl_fline', fline_tce%ntot_gl_fline
        write(50+my_rank,*) 'num_all_fline',                            &
     &                   fline_tce%num_all_fline(:,i_fln)
        write(50+my_rank,*) 'istack_all_fline',                         &
     &                   fline_tce%istack_all_fline(:,i_fln)
!
        write(50+my_rank,*) 'num_line_local',                           &
     &                  fline_src%num_line_local(i_fln)
        do i = 1, fline_src%num_line_local(i_fln)
          write(50+my_rank,*) 'id_surf_start_fline', i,                 &
     &                  fline_prm%id_surf_start_fline(1:2,i+ist_line)
          write(50+my_rank,'(a,1p4e16.5)') 'start_point, flux',         &
     &                  fline_src%xx_start_fline(1:3,i+ist_line),       &
     &                  fline_src%flux_start_fline(i+ist_line)
        end do
!
        ist = fline_tce%istack_all_fline(my_rank,i_fln) + 1
        ied = fline_tce%istack_all_fline(my_rank+1,i_fln)
        do i = ist, ied
          write(50+my_rank,*) 'isf_fline_start', i,                     &
     &                         fline_tce%isf_fline_start(1:3,i)
          write(50+my_rank,'(a,1p3e16.5)') 'start_point',               &
     &      fline_tce%xx_fline_start(1:3,i)
        end do
      end if
!
!
      end subroutine s_start_surface_4_fline
!
!  ---------------------------------------------------------------------
!
      end module start_surface_4_fline
