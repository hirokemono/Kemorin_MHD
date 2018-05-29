!start_surface_by_flux.f90
!
!      module start_surface_by_flux
!
!      Written by H. Matsui on Aug., 2011
!
!!      subroutine s_start_surface_by_flux(i_fln, node, ele, surf,      &
!!     &          fline_prm, fline_src, fline_tce)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(fieldline_paramters), intent(inout) :: fline_prm
!!        type(fieldline_source), intent(inout) :: fline_src
!!        type(fieldline_trace), intent(inout) :: fline_tce
!
      module start_surface_by_flux
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
      private :: cal_flux_for_1sgrp
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_start_surface_by_flux(i_fln, node, ele, surf,        &
     &          fline_prm, fline_src, fline_tce)
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
      type(fieldline_paramters), intent(inout) :: fline_prm
      type(fieldline_source), intent(inout) :: fline_src
      type(fieldline_trace), intent(inout) :: fline_tce
!
      integer(kind = kint) :: ist_grp, num_grp, i, ip, inum
      integer(kind = kint) :: ist_line, num
      real(kind = kreal) :: flux, flux_new
!
      real(kind = 8), allocatable :: r_rnd(:)
      real(kind = kreal), allocatable :: rnd_flux(:)
!
      real(kind = kreal) :: tot_flux_start, tot_flux_start_l
      real(kind = kreal) :: abs_flux_start, abs_flux_start_l
      real(kind = kreal) :: flux_4_each_line
!
      integer(kind = 4) :: nRand = 2
      integer(kind = 4) ::  count, clock
      integer(kind = 4), allocatable :: seed(:)
!
!
      ist_grp = fline_src%istack_ele_start_grp(i_fln-1) + 1
      num_grp = fline_src%nele_start_grp(i_fln)
!
      call cal_flux_for_1sgrp(node%numnod, ele%numele, surf%numsurf,    &
     &    surf%nnod_4_surf, surf%ie_surf, surf%isf_4_ele,               &
     &    ele%interior_ele, surf%vnorm_surf, surf%area_surf, num_grp,   &
     &    fline_src%iele_start_item(1,ist_grp),                         &
     &    fline_src%vector_nod_fline(1,1,i_fln),                        &
     &    fline_src%flux_start(ist_grp) )
!
      abs_flux_start_l = 0.0d0
      tot_flux_start_l = 0.0d0
      do i = ist_grp, fline_src%istack_ele_start_grp(i_fln)
        abs_flux_start_l                                                &
     &            = abs_flux_start_l + abs(fline_src%flux_start(i))
        tot_flux_start_l                                                &
     &            = tot_flux_start_l + fline_src%flux_start(i)
      end do
!
      call MPI_allREDUCE(tot_flux_start_l, tot_flux_start, ione,        &
     &      CALYPSO_REAL, MPI_SUM, CALYPSO_COMM, ierr_MPI)
      call MPI_AllGather(abs_flux_start_l, ione,                        &
     &      CALYPSO_REAL, fline_tce%flux_stack_fline(1), ione,          &
     &      CALYPSO_REAL, CALYPSO_COMM, ierr_MPI)
!
      fline_tce%flux_stack_fline(0) = 0.0d0
      do ip = 1, nprocs
        fline_tce%flux_stack_fline(ip)                                  &
     &                         = fline_tce%flux_stack_fline(ip-1)       &
     &                          + fline_tce%flux_stack_fline(ip)
      end do
      abs_flux_start = fline_tce%flux_stack_fline(nprocs)
      flux_4_each_line = abs_flux_start                                 &
     &                    / dble(fline_prm%num_each_field_line(i_fln))
!
      do ip = 1, nprocs
        fline_tce%num_all_fline(ip,i_fln)                               &
     &     = nint(fline_tce%flux_stack_fline(ip) / flux_4_each_line)
      end do
      fline_src%num_line_local(i_fln)                                   &
     &     = fline_tce%num_all_fline(my_rank+1,i_fln)                   &
     &      - fline_tce%num_all_fline(my_rank,i_fln)
!
      if(i_debug .gt. iflag_full_msg) then
        write(my_rank+50,*)  'abs_flux_start',                          &
     &                      abs_flux_start_l, abs_flux_start
        write(my_rank+50,*)  'tot_flux_start',                          &
     &                      tot_flux_start_l, tot_flux_start
        write(my_rank+50,*)  'original num_each_field_line',            &
     &                    i_fln, fline_src%num_line_local(i_fln)
        write(my_rank+50,*)  'flux_4_each_line', flux_4_each_line
      end if
!
      if(fline_src%num_line_local(i_fln) .gt. 0) then
        flux_4_each_line = abs_flux_start_l                             &
     &                      / dble(fline_src%num_line_local(i_fln) )
      end if
      write(my_rank+50,*)  'adjusted flux_4_each_line',                 &
     &                     flux_4_each_line
!
      ist_line = fline_prm%istack_each_field_line(i_fln-1)
      inum = ist_grp
!
!
      write(my_rank+50,*)  'random_seed',                               &
     &                      nRand, fline_src%num_line_local(i_fln)
      call random_seed(size = nRand)
!
      num = fline_src%num_line_local(i_fln)
      seed = clock
      allocate(seed(nRand))
      allocate(r_rnd(num))
      allocate(rnd_flux(num))
!
      if(num .gt. 0) then
        write(*,*)  'system_clock'
        call system_clock(count = clock)
        write(*,*)  'random_seed'
        call random_seed(put = seed)
        write(*,*)  'random_number'
        call random_number(r_rnd) 
        do i = 1, fline_src%num_line_local(i_fln)
          rnd_flux(i) = r_rnd(i) * abs_flux_start_l
!
          flux = 0.0d0
          do inum = ist_grp+1, fline_src%istack_ele_start_grp(i_fln)
            flux_new = flux + abs(fline_src%flux_start(inum))
            if(rnd_flux(i) .gt. flux                                    &
     &           .and. rnd_flux(i) .le. flux_new) exit
            flux = flux_new
          end do
          fline_prm%id_surf_start_fline(1,i+ist_line)                   &
     &            = fline_src%iele_start_item(1,inum)
          fline_prm%id_surf_start_fline(2,i+ist_line)                   &
     &            = fline_src%iele_start_item(2,inum)
        end do
      end if
!
      deallocate(rnd_flux, r_rnd, seed)
!
      end subroutine s_start_surface_by_flux
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine cal_flux_for_1sgrp(numnod, numele, numsurf,            &
     &          nnod_4_surf, ie_surf, isf_4_ele, interior_ele,          &
     &          vnorm_surf, area_surf, num_sgrp,                        &
     &          isurf_grp, d_nod, flux)
!
      use m_geometry_constants
!
      integer(kind = kint), intent(in) :: numnod, numele, numsurf
      integer(kind = kint), intent(in) :: nnod_4_surf
      integer(kind = kint), intent(in) :: ie_surf(numsurf,nnod_4_surf)
      integer(kind = kint), intent(in) :: isf_4_ele(numele,nsurf_4_ele)
      integer(kind = kint), intent(in) :: num_sgrp
      integer(kind = kint), intent(in) :: isurf_grp(2,num_sgrp)
      integer(kind = kint), intent(in) :: interior_ele(numele)
      real(kind = kreal), intent(in) :: vnorm_surf(numsurf,3)
      real(kind = kreal), intent(in) :: area_surf(numsurf)
      real(kind = kreal), intent(in) :: d_nod(numnod,3)
!
      real(kind = kreal), intent(inout) :: flux(num_sgrp)
!
      integer (kind = kint) :: iele, isf, isurf, inum
      integer (kind = kint) :: i1,  i2,  i3,  i4
      real(kind = kreal) :: sign_surf, d_surf(3)
!
!
      flux(1:num_sgrp) = 0.0d0
!
!$omp  parallel do                                                      &
!$omp& private(inum,iele,isf,isurf,sign_surf,i1,i2,i3,i4,d_surf)
!$cdir nodep
      do inum = 1, num_sgrp
        iele = isurf_grp(1,inum)
        isf =  isurf_grp(2,inum)
        isurf = abs(isf_4_ele(iele,isf))
        sign_surf = dble(isf_4_ele(iele,isf) / isurf)
!
        i1 =  ie_surf(isurf, 1)
        i2 =  ie_surf(isurf, 2)
        i3 =  ie_surf(isurf, 3)
        i4 =  ie_surf(isurf, 4)
!
        d_surf(1) = quad * (d_nod(i1,1) + d_nod(i2,1)                   &
     &                    + d_nod(i3,1) + d_nod(i4,1))
        d_surf(2) = quad * (d_nod(i1,2) + d_nod(i2,2)                   &
     &                    + d_nod(i3,2) + d_nod(i4,2))
        d_surf(3) = quad * (d_nod(i1,3) + d_nod(i2,3)                   &
     &                    + d_nod(i3,3) + d_nod(i4,3))
!
        flux(inum) = flux(inum) + ( vnorm_surf(isurf,1) * d_surf(1)     &
     &                            + vnorm_surf(isurf,2) * d_surf(2)     &
     &                            + vnorm_surf(isurf,3) * d_surf(3) )   &
     &              * area_surf(isurf) * sign_surf                      &
     &              * dble(interior_ele(iele))
      end do
!$omp end parallel do
!
      end subroutine cal_flux_for_1sgrp
!
!  ---------------------------------------------------------------------
!
      end module start_surface_by_flux
