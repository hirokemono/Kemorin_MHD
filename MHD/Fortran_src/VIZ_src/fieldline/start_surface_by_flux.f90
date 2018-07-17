!start_surface_by_flux.f90
!
!      module start_surface_by_flux
!
!      Written by H. Matsui on Aug., 2011
!
!!      subroutine s_start_surface_by_flux(i_fln, node, ele, surf,      &
!!     &          fln_prm, fline_prm, fline_src, fln_tce)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(fieldline_paramter), intent(in) :: fln_prm
!!        type(fieldline_paramters), intent(inout) :: fline_prm
!!        type(all_fieldline_source), intent(inout) :: fline_src
!!        type(each_fieldline_trace), intent(inout) :: fln_tce
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
      private :: cal_flux_for_1sgrp, cal_area_for_1sgrp
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_start_surface_by_flux(i_fln, node, ele, surf,        &
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
      integer(kind = kint) :: ist_grp, num_grp, i, ip
      integer(kind = kint) :: ist_line, num_line
!
      real(kind = kreal) :: tot_flux_start, tot_flux_start_l
      real(kind = kreal) :: abs_flux_start, abs_flux_start_l
      real(kind = kreal) :: flux_4_each_line
!
!
      ist_grp = fline_src%istack_ele_start_grp(i_fln-1) + 1
      num_grp = fline_src%nele_start_grp(i_fln)
      call calypso_mpi_barrier
!
      if(     fln_prm%id_seed_distribution .eq. iflag_random_by_area    &
     &   .or. fln_prm%id_seed_distribution .eq. iflag_no_random) then
        if(iflag_debug .gt. 0) write(*,*) 'cal_area_for_1sgrp'
        call cal_area_for_1sgrp(ele%numele, surf%numsurf,               &
     &      surf%isf_4_ele, ele%interior_ele, surf%area_surf,           &
     &      num_grp, fline_src%iele_start_item(1,ist_grp),              &
     &      fline_src%flux_start(ist_grp) )
      else
        if(iflag_debug .gt. 0) write(*,*) 'cal_flux_for_1sgrp'
        call cal_flux_for_1sgrp(node%numnod, ele%numele, surf%numsurf,  &
     &      surf%nnod_4_surf, surf%ie_surf, surf%isf_4_ele,             &
     &      ele%interior_ele, surf%vnorm_surf, surf%area_surf, num_grp, &
     &      fline_src%iele_start_item(1,ist_grp),                       &
     &      fln_src%vector_nod_fline, fline_src%flux_start(ist_grp) )
      end if
      call calypso_mpi_barrier
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
     &      CALYPSO_REAL, fln_tce%flux_stack_fline(1), ione,            &
     &      CALYPSO_REAL, CALYPSO_COMM, ierr_MPI)
!
      fln_tce%flux_stack_fline(0) = 0.0d0
      do ip = 1, nprocs
        fln_tce%flux_stack_fline(ip)                                    &
     &                         = fln_tce%flux_stack_fline(ip-1)         &
     &                          + fln_tce%flux_stack_fline(ip)
      end do
      abs_flux_start = fln_tce%flux_stack_fline(nprocs)
      flux_4_each_line = abs_flux_start                                 &
     &                    / dble(fline_prm%num_each_field_line(i_fln))
!
      do ip = 1, nprocs
        fln_tce%num_current_fline(ip)                                   &
     &     = nint((fln_tce%flux_stack_fline(ip)                         &
     &      - fln_tce%flux_stack_fline(ip-1)) / flux_4_each_line)
      end do
      fln_src%num_line_local = fln_tce%num_current_fline(my_rank)
!
      if(i_debug .gt. 0) then
        write(my_rank+50,*)  'abs_flux_start',                          &
     &                      abs_flux_start_l, abs_flux_start
        write(my_rank+50,*)  'tot_flux_start',                          &
     &                      tot_flux_start_l, tot_flux_start
        write(my_rank+50,*)  'original num_each_field_line',            &
     &                      fln_src%num_line_local
        write(my_rank+50,*)  'flux_4_each_line', flux_4_each_line
      end if
!
      if(fln_src%num_line_local .gt. 0) then
        flux_4_each_line                                                &
     &       = abs_flux_start_l / dble(fln_src%num_line_local)
      end if
      write(my_rank+50,*)  'adjusted flux_4_each_line',                 &
     &                     flux_4_each_line
      call calypso_mpi_barrier
!
      ist_line = fline_prm%istack_each_field_line(i_fln-1) + 1
      num_line = fline_prm%istack_each_field_line(i_fln) - ist_line
      if(num_line .gt. 0) then
        if(fln_prm%id_seed_distribution  .eq. iflag_no_random) then
          if(iflag_debug .gt. 0) write(*,*) 'start_surface_witout_random'
          call start_surface_witout_random                              &
     &       (i_fln, fline_src, fln_src, abs_flux_start_l,              &
     &        num_line, fline_prm%id_surf_start_fline(1,ist_line))
        else
          if(iflag_debug .gt. 0) write(*,*) 'start_surface_by_random'
          call start_surface_by_random                                  &
     &       (i_fln, fline_src, fln_src, abs_flux_start_l,              &
     &        num_line, fline_prm%id_surf_start_fline(1, ist_line))
        end if
      end if
!
      write(*,*) 'calypso_mpi_barrier'
      call calypso_mpi_barrier
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
      subroutine cal_area_for_1sgrp                                     &
     &         (numele, numsurf, isf_4_ele, interior_ele, area_surf,    &
     &          num_sgrp, isurf_grp, flux)
!
      use m_geometry_constants
!
      integer(kind = kint), intent(in) :: numele, numsurf
      integer(kind = kint), intent(in) :: isf_4_ele(numele,nsurf_4_ele)
      integer(kind = kint), intent(in) :: num_sgrp
      integer(kind = kint), intent(in) :: isurf_grp(2,num_sgrp)
      integer(kind = kint), intent(in) :: interior_ele(numele)
      real(kind = kreal), intent(in) :: area_surf(numsurf)
!
      real(kind = kreal), intent(inout) :: flux(num_sgrp)
!
      integer (kind = kint) :: iele, isf, isurf, inum
!
!
      flux(1:num_sgrp) = 0.0d0
!
!$omp  parallel do private(inum,iele,isf,isurf)
      do inum = 1, num_sgrp
        iele = isurf_grp(1,inum)
        isf =  isurf_grp(2,inum)
        isurf = abs(isf_4_ele(iele,isf))
!
        flux(inum) = flux(inum)                                         &
     &              + area_surf(isurf)  * dble(interior_ele(iele))
      end do
!$omp end parallel do
!
      end subroutine cal_area_for_1sgrp
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine start_surface_by_random                                &
     &         (i_fln, fline_src, fln_src, abs_flux_start_l,            &
     &          num_line, id_surf_start_fline)
!
      use extend_field_line
      use cal_field_on_surf_viz
      use set_fline_start_surface
!
      type(each_fieldline_source), intent(in) :: fln_src
      type(all_fieldline_source), intent(in) :: fline_src
      real(kind = kreal), intent(in) :: abs_flux_start_l
      integer(kind = kint), intent(in) :: i_fln
!
      integer(kind = kint), intent(in) :: num_line
!
      integer(kind = kint), intent(inout)                               &
     &                          :: id_surf_start_fline(2,num_line)
!
      integer(kind = kint) :: ist_grp, i, inum, num
      real(kind = kreal) :: flux, flux_new
!
!
      real(kind = 8), allocatable :: r_rnd(:)
      real(kind = kreal), allocatable :: rnd_flux(:)
!
      integer(kind = 4) :: nRand = 2
      integer(kind = 4) ::  count, clock
      integer(kind = 4), allocatable :: seed(:)
!
!
      ist_grp = fline_src%istack_ele_start_grp(i_fln-1) + 1
!
      write(my_rank+50,*)  'random_seed',                               &
     &                      nRand, fln_src%num_line_local
      call random_seed(size = nRand)
!
      num = fln_src%num_line_local
      allocate(seed(nRand))
      allocate(r_rnd(num))
      allocate(rnd_flux(num))
!
      if(iflag_debug .gt. 0) write(*,*)  'system_clock', num
      call system_clock(count = clock)
      seed = clock
!
      if(num .gt. 0) then
        if(iflag_debug .gt. 0) write(*,*)  'random_seed'
        call random_seed(put = seed)
        if(iflag_debug .gt. 0) write(*,*)  'random_number'
        call random_number(r_rnd) 
        do i = 1, fln_src%num_line_local
          rnd_flux(i) = r_rnd(i) * abs_flux_start_l
!
          flux = 0.0d0
          do inum = ist_grp, fline_src%istack_ele_start_grp(i_fln)
            flux_new = flux + abs(fline_src%flux_start(inum))
            if(rnd_flux(i) .gt. flux                                    &
     &           .and. rnd_flux(i) .le. flux_new) exit
            flux = flux_new
          end do
          id_surf_start_fline(1,i) = fline_src%iele_start_item(1,inum)
          id_surf_start_fline(2,i) = fline_src%iele_start_item(2,inum)
        end do
      end if
!
      deallocate(rnd_flux, r_rnd, seed)
      call calypso_mpi_barrier
!
      end subroutine start_surface_by_random
!
!  ---------------------------------------------------------------------
!
      subroutine start_surface_witout_random                            &
     &         (i_fln, fline_src, fln_src, abs_flux_start_l,            &
     &          num_line, id_surf_start_fline)
!
      use extend_field_line
      use cal_field_on_surf_viz
      use set_fline_start_surface
!
      type(each_fieldline_source), intent(in) :: fln_src
      type(all_fieldline_source), intent(in) :: fline_src
      real(kind = kreal), intent(in) :: abs_flux_start_l
      integer(kind = kint), intent(in) :: i_fln
      integer(kind = kint), intent(in) :: num_line
!
      integer(kind = kint), intent(inout)                               &
     &                          :: id_surf_start_fline(2,num_line)
!
      integer(kind = kint) :: ist_grp, ied_grp, icou, inum
      real(kind = kreal) :: flux, ref_flux
!
!
      ref_flux = abs_flux_start_l / num_line
      ist_grp = fline_src%istack_ele_start_grp(i_fln-1) + 1
      ied_grp = fline_src%istack_ele_start_grp(i_fln)
      icou = 0
      if(fln_src%num_line_local .gt. 0) then
        flux = 0.0d0
        do inum = ist_grp, ied_grp
          flux = flux + abs(fline_src%flux_start(inum))
          if(flux .gt. ref_flux) then
            icou = icou + 1
            id_surf_start_fline(1,icou)                                &
     &               = fline_src%iele_start_item(1,inum)
            id_surf_start_fline(2,icou)                                &
     &               = fline_src%iele_start_item(2,inum)
            flux = 0.0d0
          end if
          if(icou .ge. num_line) exit
        end do
      end if
!
      end subroutine start_surface_witout_random
!
!  ---------------------------------------------------------------------
!
      end module start_surface_by_flux
