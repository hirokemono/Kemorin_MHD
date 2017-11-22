!set_fields_for_fieldline.f90
!
!      module set_fields_for_fieldline
!
!      Written by H. Matsui on Aug., 2011
!
!!      subroutine set_local_field_4_fline(node, nod_fld, fline_src)
!!      subroutine count_nsurf_for_starting                             &
!!     &         (i_fln, ele, sf_grp, fline_src)
!!      subroutine set_isurf_for_starting(i_fln, ele, sf_grp, fline_src)
!!      subroutine s_set_fields_for_fieldline                           &
!!     &         (i_fln, node, ele, surf, ele_grp, fline_src, fline_tce)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(group_data), intent(in) :: ele_grp
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(fieldline_source), intent(inout) :: fline_src
!!        type(fieldline_trace), intent(inout) :: fline_tce
!
      module set_fields_for_fieldline
!
      use m_precision
!
      use calypso_mpi
      use m_constants
      use m_machine_parameter
      use m_control_params_4_fline
!
      use t_phys_data
      use t_geometry_data
      use t_surface_data
      use t_group_data
!
      implicit  none
!
      private :: cnt_start_surface_by_gl_table, cal_flux_for_1sgrp
      private :: set_start_surface_by_gl_table
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_local_field_4_fline(node, nod_fld, fline_src)
!
      use t_source_of_filed_line
      use convert_components_4_viz
!
      type(node_data), intent(in) :: node
      type(phys_data), intent(in) :: nod_fld
!
      type(fieldline_source), intent(inout) :: fline_src
!
      integer(kind = kint) :: i_fln
      integer(kind = kint) :: i_field, ist_fld, num_comp
!
!
      do i_fln = 1, num_fline
        i_field = ifield_4_fline(i_fln)
        ist_fld = nod_fld%istack_component(i_field-1)
        num_comp = nod_fld%istack_component(i_field) - ist_fld
!
        if (iflag_debug .gt. 0) write(*,*)                              &
     &    'convert_comps_4_viz ifield_4_fline', i_field
        call convert_comps_4_viz(node%numnod, node%istack_nod_smp,      &
     &      node%xx, node%rr,node%a_r, node%ss, node%a_s, ithree,       &
     &      num_comp, icomp_4_fline(i_fln), nod_fld%d_fld(1,ist_fld+1), &
     &      fline_src%vector_nod_fline(1,1,i_fln) )
!
        i_field = ifield_linecolor(i_fln)
        ist_fld = nod_fld%istack_component(i_field-1)
        num_comp = nod_fld%istack_component(i_field) - ist_fld
        if (iflag_debug .gt. 0) write(*,*)                              &
     &     'convert_comps_4_viz ifield_linecolor', i_field
        call convert_comps_4_viz(node%numnod, node%istack_nod_smp,      &
     &      node%xx, node%rr, node%a_r, node%ss, node%a_s, ione,        &
     &      num_comp, icomp_linecolor(i_fln),                           &
     &      nod_fld%d_fld(1,ist_fld+1),                                 &
     &      fline_src%color_nod_fline(1,i_fln) )
      end do
!
!
      end subroutine set_local_field_4_fline
!
!  ---------------------------------------------------------------------
!
      subroutine count_nsurf_for_starting                               &
     &         (i_fln, ele, sf_grp, fline_src)
!
      use t_source_of_filed_line
!
      integer(kind = kint), intent(in) :: i_fln
!
      type(element_data), intent(in) :: ele
      type(surface_group_data), intent(in) :: sf_grp
!
      type(fieldline_source), intent(inout) :: fline_src
!
      integer(kind = kint) :: igrp, isurf, iele, icou, ist, ied
!
!
      igrp = igrp_start_fline_surf_grp(i_fln)
!
      icou = 0
      ist = sf_grp%istack_grp(igrp-1) + 1
      ied = sf_grp%istack_grp(igrp)
      do isurf = ist, ied
        iele = sf_grp%item_sf_grp(1,isurf)
        if(ele%interior_ele(iele) .ne. izero) icou = icou + 1
      end do
!
      fline_src%nele_start_grp(i_fln) = icou
      fline_src%istack_ele_start_grp(i_fln)                             &
     &             = fline_src%istack_ele_start_grp(i_fln-1)            &
     &              + fline_src%nele_start_grp(i_fln)
!
!
      end subroutine count_nsurf_for_starting
!
!  ---------------------------------------------------------------------
!
      subroutine set_isurf_for_starting(i_fln, ele, sf_grp, fline_src)
!
      use t_source_of_filed_line
!
      integer(kind = kint), intent(in) :: i_fln
!
      type(element_data), intent(in) :: ele
      type(surface_group_data), intent(in) :: sf_grp
!
      type(fieldline_source), intent(inout) :: fline_src
!
      integer(kind = kint) :: igrp, isurf, inum, iele, ist, ied
!
!
      igrp = igrp_start_fline_surf_grp(i_fln)
!
      inum = fline_src%istack_ele_start_grp(i_fln-1)
      ist = sf_grp%istack_grp(igrp-1) + 1
      ied = sf_grp%istack_grp(igrp)
      do isurf = ist, ied
        iele = sf_grp%item_sf_grp(1,isurf)
        if(ele%interior_ele(iele) .ne. izero) then
          inum = inum + 1
          fline_src%iele_start_item(1,inum)                             &
     &         = sf_grp%item_sf_grp(1,isurf)
          fline_src%iele_start_item(2,inum)                             &
     &         = sf_grp%item_sf_grp(2,isurf)
        end if
      end do
!
      end subroutine set_isurf_for_starting
!
!  ---------------------------------------------------------------------
!
      subroutine s_set_fields_for_fieldline                             &
     &         (i_fln, node, ele, surf, ele_grp, fline_src, fline_tce)
!
      use extend_field_line
      use cal_field_on_surf_viz
      use set_fline_start_surface
      use t_source_of_filed_line
!
      integer(kind = kint), intent(in) :: i_fln
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(group_data), intent(in) :: ele_grp
!
      type(fieldline_source), intent(inout) :: fline_src
      type(fieldline_trace), intent(inout) :: fline_tce
!
      integer(kind = kint) :: ist_grp, num_grp, i, ist, ied, ip, inum
      integer(kind = kint) :: ist_line, iele, isf, isurf, num
      real(kind = kreal) :: flux, flux_new, vec_surf(3), xi(2)
!
      real(kind = 8), allocatable :: r_rnd(:)
      real(kind = kreal), allocatable :: rnd_flux(:)
!
      real(kind = kreal) :: tot_flux_start, tot_flux_start_l
      real(kind = kreal) :: abs_flux_start, abs_flux_start_l
      real(kind = kreal) :: flux_4_each_line
!
      integer(kind = kint) :: num_int_point
!
      integer(kind = 4) :: nRand = 2
      integer(kind = 4) ::  count, clock
      integer(kind = 4), allocatable :: seed(:)
!
!
      num_int_point = 2
      if(id_fline_start_type(i_fln) .eq. 0) then
        ist_grp = fline_src%istack_ele_start_grp(i_fln-1) + 1
        num_grp = fline_src%nele_start_grp(i_fln)
!
        call cal_flux_for_1sgrp(node%numnod, ele%numele, surf%numsurf,  &
     &      surf%nnod_4_surf, surf%ie_surf, surf%isf_4_ele,             &
     &      ele%interior_ele, surf%vnorm_surf, surf%area_surf, num_grp, &
     &      fline_src%iele_start_item(1,ist_grp),                       &
     &      fline_src%vector_nod_fline(1,1,i_fln),                      &
     &      fline_src%flux_start(ist_grp) )
!
        abs_flux_start_l = 0.0d0
        tot_flux_start_l = 0.0d0
        do i = ist_grp, fline_src%istack_ele_start_grp(i_fln)
          abs_flux_start_l                                              &
     &            = abs_flux_start_l + abs(fline_src%flux_start(i))
          tot_flux_start_l                                              &
     &            = tot_flux_start_l + fline_src%flux_start(i)
        end do
!
        call MPI_allREDUCE(tot_flux_start_l, tot_flux_start, ione,      &
     &      CALYPSO_REAL, MPI_SUM, CALYPSO_COMM, ierr_MPI)
        call MPI_AllGather(abs_flux_start_l, ione,                      &
     &      CALYPSO_REAL, fline_tce%flux_stack_fline(1), ione,          &
     &      CALYPSO_REAL, CALYPSO_COMM, ierr_MPI)
!
        fline_tce%flux_stack_fline(0) = 0.0d0
        do ip = 1, nprocs
          fline_tce%flux_stack_fline(ip)                                &
     &                         = fline_tce%flux_stack_fline(ip-1)       &
     &                          + fline_tce%flux_stack_fline(ip)
        end do
        abs_flux_start = fline_tce%flux_stack_fline(nprocs)
        flux_4_each_line = abs_flux_start                               &
     &                    / dble(num_each_field_line(i_fln) )
!
        do ip = 1, nprocs
          fline_tce%num_all_fline(ip,i_fln)                             &
     &       = nint(fline_tce%flux_stack_fline(ip)                      &
     &              / flux_4_each_line)
        end do
        fline_src%num_line_local(i_fln)                                 &
     &       = fline_tce%num_all_fline(my_rank+1,i_fln)                 &
     &        - fline_tce%num_all_fline(my_rank,i_fln)
!
        if(i_debug .gt. iflag_full_msg) then
          write(my_rank+50,*)  'abs_flux_start',                        &
     &                        abs_flux_start_l, abs_flux_start
          write(my_rank+50,*)  'tot_flux_start',                        &
     &                        tot_flux_start_l, tot_flux_start
          write(my_rank+50,*)  'original num_each_field_line',          &
     &                      i_fln, fline_src%num_line_local(i_fln)
          write(my_rank+50,*)  'flux_4_each_line', flux_4_each_line
        end if
!
        if(fline_src%num_line_local(i_fln) .gt. 0) then
          flux_4_each_line = abs_flux_start_l                           &
     &                      / dble(fline_src%num_line_local(i_fln) )
        end if
        write(my_rank+50,*)  'adjusted flux_4_each_line',               &
     &                     flux_4_each_line
!
        ist_line = istack_each_field_line(i_fln-1)
        inum = ist_grp
!
!
        write(my_rank+50,*)  'random_seed',                             &
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
              if(rnd_flux(i) .gt. flux                                  &
     &           .and. rnd_flux(i) .le. flux_new) exit
              flux = flux_new
            end do
!
            id_surf_start_fline(1,i+ist_line)                           &
     &            = fline_src%iele_start_item(1,inum)
            id_surf_start_fline(2,i+ist_line)                           &
     &            = fline_src%iele_start_item(2,inum)
!
          end do
        end if
!
        deallocate(rnd_flux, r_rnd, seed)
!
      else if(id_fline_start_type(i_fln) .eq. 1) then
        call cnt_start_surface_by_gl_table                              &
     &     (i_fln, ele%numele, ele%iele_global,                         &
     &      ele%interior_ele, ele_grp%num_grp, ele_grp%num_item,        &
     &      ele_grp%istack_grp, ele_grp%item_grp, fline_src)
        call set_start_surface_by_gl_table                              &
     &     (i_fln, ele%numele, ele%iele_global,                         &
     &      ele%interior_ele, ele_grp%num_grp, ele_grp%num_item,        &
     &      ele_grp%istack_grp, ele_grp%item_grp, fline_src)
      end if
!
      ist_line = istack_each_field_line(i_fln-1)
      do i = 1, fline_src%num_line_local(i_fln)
        inum = i + ist_line
        iele = id_surf_start_fline(1,inum)
        isf =  id_surf_start_fline(2,inum)
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
          iflag_outward_flux_fline(inum) = 1
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
      if( id_fline_direction(i_fln) .eq. 0) then
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
     &    fline_src, fline_tce)
!
      if(i_debug .gt. iflag_full_msg) then
        write(50+my_rank,*) 'ntot_gl_fline', fline_tce%ntot_gl_fline
        write(50+my_rank,*) 'ntot_gl_fline', fline_tce%ntot_gl_fline
        write(50+my_rank,*) 'num_all_fline',                            &
     &                     fline_tce%num_all_fline(:,i_fln)
        write(50+my_rank,*) 'istack_all_fline',                         &
     &                     fline_tce%istack_all_fline(:,i_fln)
!
        write(50+my_rank,*) 'num_line_local',                           &
     &                     fline_src%num_line_local(i_fln)
        do i = 1, fline_src%num_line_local(i_fln)
          write(50+my_rank,*) 'id_surf_start_fline', i,                 &
     &                        id_surf_start_fline(1:2,i+ist_line)
          write(50+my_rank,'(a,1p4e16.5)') 'start_point, flux',         &
     &                        fline_src%xx_start_fline(1:3,i+ist_line), &
     &                        fline_src%flux_start_fline(i+ist_line)
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
      end subroutine s_set_fields_for_fieldline
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine cnt_start_surface_by_gl_table(i_fln, numele,           &
     &          iele_global, interior_ele, num_mat, num_mat_bc,         &
     &          mat_istack, mat_item, fline_src)
!
      use t_source_of_filed_line
!
      integer(kind = kint), intent(in) :: i_fln
!
      integer(kind=kint), intent(in) :: numele
      integer(kind=kint_gl), intent(in) :: iele_global(numele)
      integer(kind = kint), intent(in) :: interior_ele(numele)
!
      integer(kind=kint), intent(in) :: num_mat, num_mat_bc
      integer(kind=kint), intent(in) :: mat_istack(0:num_mat)
      integer(kind=kint), intent(in) :: mat_item(num_mat_bc)
!
      type(fieldline_source), intent(inout) :: fline_src
!
      integer(kind = kint) :: inum, ist_grp, ied_grp
      integer(kind = kint) :: jgrp, jst_grp, jed_grp
      integer(kind = kint) :: icou, jnum, jele, jg, jst, jed
      integer(kind = kint_gl) :: iele_g
!
!
      icou = 0
      ist_grp = fline_src%istack_ele_start_grp(i_fln-1) + 1
      ied_grp = fline_src%istack_ele_start_grp(i_fln)
      jst_grp = istack_grp_area_fline(i_fln-1) + 1
      jed_grp = istack_grp_area_fline(i_fln)
      do inum = ist_grp, ied_grp
        iele_g = id_gl_surf_start_fline(1,inum)
          do jgrp = jst_grp, jed_grp
            jg = id_ele_grp_area_fline(jgrp)
            jst = mat_istack(jg-1) + 1
            jed = mat_istack(jg)
            do jnum = jst, jed
              jele = mat_item(jnum)
              if(iele_g.eq.iele_global(jele)                            &
     &           .and. interior_ele(jele) .gt. 0) then
                icou = icou + 1
                exit
              end if
            end do
          end do
        end do
        fline_src%num_line_local(i_fln) = icou
!
      end subroutine cnt_start_surface_by_gl_table
!
!  ---------------------------------------------------------------------
!
      subroutine set_start_surface_by_gl_table(i_fln, numele,           &
     &          iele_global, interior_ele, num_mat, num_mat_bc,         &
     &          mat_istack, mat_item, fline_src)
!
      use t_source_of_filed_line
!
      integer(kind = kint), intent(in) :: i_fln
!
      integer(kind = kint), intent(in) :: numele
      integer(kind = kint_gl), intent(in) :: iele_global(numele)
      integer(kind = kint), intent(in) :: interior_ele(numele)
!
      integer(kind=kint), intent(in) :: num_mat, num_mat_bc
      integer(kind=kint), intent(in) :: mat_istack(0:num_mat)
      integer(kind=kint), intent(in) :: mat_item(num_mat_bc)
!
      type(fieldline_source), intent(in) :: fline_src
!
      integer(kind = kint) :: inum, ist_grp, ied_grp
      integer(kind = kint) :: jgrp, jst_grp, jed_grp
      integer(kind = kint) :: icou, jnum, jele, jg, jst, jed
      integer(kind = kint_gl) :: iele_g
!
!
      icou = istack_each_field_line(i_fln-1)
      ist_grp = fline_src%istack_ele_start_grp(i_fln-1) + 1
      ied_grp = fline_src%istack_ele_start_grp(i_fln)
      jst_grp = istack_grp_area_fline(i_fln-1) + 1
      jed_grp = istack_grp_area_fline(i_fln)
      do inum = ist_grp, ied_grp
        iele_g = id_gl_surf_start_fline(1,inum)
          do jgrp = jst_grp, jed_grp
            jg = id_ele_grp_area_fline(jgrp)
            jst = mat_istack(jg-1) + 1
            jed = mat_istack(jg)
            do jnum = jst, jed
              jele = mat_item(jnum)
              if(iele_g.eq.iele_global(jele)                            &
     &           .and. interior_ele(jele) .gt. 0) then
                icou = icou + 1
                id_surf_start_fline(1,icou) = jele
                id_surf_start_fline(2,icou)                             &
     &               = id_gl_surf_start_fline(2,inum)
                exit
              end if
            end do
          end do
        end do
!
      end subroutine set_start_surface_by_gl_table
!
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
      end module set_fields_for_fieldline
