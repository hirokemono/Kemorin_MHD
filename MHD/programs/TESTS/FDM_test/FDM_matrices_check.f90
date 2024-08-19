!
      program FDM_matrices_check
!
      use m_precision
      use m_constants
      use t_spheric_parameter
      use t_fdm_coefs
      use t_group_data
      use chebyshev_radial_grid
!
      implicit none
!
      type(sph_grids) :: sph1
      type(fdm_matrices) :: r_2nd_1
      type(fdm_matrices) :: r_n2e_3rd_1
      type(fdm_matrices) :: r_e2n_1st_1
!
      integer :: k
!
      sph1%sph_rj%nidx_rj(1) = 128
      sph1%sph_rj%nidx_rj(2) =   6
      sph1%sph_rj%nnod_rj =    sph1%sph_rj%nidx_rj(1)                   &
     &                        * sph1%sph_rj%nidx_rj(2)
      call alloc_sph_1d_index_rj(sph1%sph_rj)
!
      sph1%sph_params%nlayer_ICB =   24
      sph1%sph_params%nlayer_CMB =   96
      sph1%sph_params%radius_ICB =   7.0d0 / 13.0d0
      sph1%sph_params%radius_CMB =   20.0d0 / 13.0d0
!
      call set_chebyshev_distance_shell(sph1%sph_rj%nidx_rj(1),         &
     &    sph1%sph_params%nlayer_ICB, sph1%sph_params%nlayer_CMB,       &
     &    sph1%sph_params%radius_ICB, sph1%sph_params%radius_CMB,       &
     &    sph1%sph_rj%radius_1d_rj_r)
!
!      do k = 1, sph1%sph_rj%nidx_rj(1)
!        write(*,*) k, sph1%sph_rj%radius_1d_rj_r(k)
!      end do
!
      call init_FDM_coefs_for_test                                      &
     &   (sph1, r_2nd_1, r_n2e_3rd_1, r_e2n_1st_1)
      call test_radial_FDM                                              &
     &   (sph1%sph_params%nlayer_ICB, sph1%sph_params%nlayer_CMB,       &
     &    sph1%sph_rj, r_2nd_1, r_n2e_3rd_1, r_e2n_1st_1)
!
!  -------------------------------------------------------------------
!
      contains
!
!  -------------------------------------------------------------------
!
      subroutine init_FDM_coefs_for_test                                &
     &         (sph, r_2nd, r_n2e_3rd, r_e2n_1st)
!
      use parallel_load_data_4_sph
      use init_radial_infos_sph_mhd
      use second_fdm_node_coefs
      use third_fdm_node_to_ele
      use first_fdm_ele_to_node
!
      type(sph_grids), intent(inout) :: sph
      type(fdm_matrices), intent(inout) :: r_2nd
      type(fdm_matrices), intent(inout) :: r_n2e_3rd
      type(fdm_matrices), intent(inout) :: r_e2n_1st
!
      if (iflag_debug.gt.0) write(*,*) 'set_delta_r_4_sph_mhd'
      call set_delta_r_4_sph_mhd(sph%sph_params, sph%sph_rj)
!
      if (iflag_debug.gt.0) write(*,*) 'const_second_fdm_coefs'
      call const_second_fdm_coefs(sph%sph_params, sph%sph_rj, r_2nd)
      call check_fdm_coefs                                            &
     &   (sph%sph_rj%nidx_rj(1), sph%sph_rj%radius_1d_rj_r, r_2nd)
!
      if (iflag_debug.gt.0) write(*,*) 'const_first_fdm_ele_to_node'
      call const_first_fdm_ele_to_node(sph%sph_rj, r_e2n_1st)
      if (iflag_debug.gt.0) write(*,*) 'const_third_fdm_node_to_ele'
      call const_third_fdm_node_to_ele(sph%sph_rj, r_n2e_3rd)
!
      end subroutine init_FDM_coefs_for_test
!
!  -------------------------------------------------------------------
!
      subroutine test_radial_FDM(kr_in, kr_out, sph_rj,                 &
     &                           r_2nd, r_n2e_3rd, r_e2n_1st)
!
      use second_fdm_node_coefs
      use third_fdm_node_to_ele
      use first_fdm_ele_to_node
!
      integer(kind = kint), intent(in) :: kr_in, kr_out
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      type(fdm_matrices), intent(in) :: r_n2e_3rd
      type(fdm_matrices), intent(in) :: r_e2n_1st
!
      real(kind = kreal), allocatable :: r_ele(:)
!
      real(kind = kreal), allocatable :: d_rj(:)
      real(kind = kreal), allocatable :: dr_rj(:)
      real(kind = kreal), allocatable :: d2r_rj(:)
!
      real(kind = kreal), allocatable :: de_rj(:)
      real(kind = kreal), allocatable :: dre_rj(:)
      real(kind = kreal), allocatable :: d2re_rj(:)
      real(kind = kreal), allocatable :: d3re_rj(:)
!
      real(kind = kreal), allocatable :: dfdr_nod(:)
      real(kind = kreal), allocatable :: d2fdr2_nod(:)
!
      real(kind = kreal), allocatable :: d_ele(:)
      real(kind = kreal), allocatable :: dfdr_ele(:)
      real(kind = kreal), allocatable :: d2fdr2_ele(:)
      real(kind = kreal), allocatable :: d3fdr3_ele(:)
!
      real(kind = kreal), allocatable :: d_e2n(:)
      real(kind = kreal), allocatable :: dfdr_e2n(:)
!
      integer(kind = kint) :: inod, j, k
      real(kind = kreal) :: r
!
      allocate(r_ele(sph_rj%nidx_rj(1)))
      do k = 1, sph_rj%nidx_rj(1)
        if(k .eq. 1) then
          r_ele(k) = half * sph_rj%radius_1d_rj_r(k)
        else
          r_ele(k) = half * (sph_rj%radius_1d_rj_r(k-1)                 &
     &                      + sph_rj%radius_1d_rj_r(k))
        end if
      end do
!
      allocate(d_rj(sph_rj%nnod_rj))
      allocate(dr_rj(sph_rj%nnod_rj))
      allocate(d2r_rj(sph_rj%nnod_rj))
      do inod = 1, sph_rj%nnod_rj
        j = mod((inod-1),sph_rj%nidx_rj(2)) + 1
        k = 1 + (inod- j) / sph_rj%nidx_rj(2)
        d_rj(inod) =   sph_rj%radius_1d_rj_r(k)**j
        dr_rj(inod) =  dble(j) * sph_rj%radius_1d_rj_r(k)**(j-1)
        d2r_rj(inod) = dble(j*(j-1)) * sph_rj%radius_1d_rj_r(k)**(j-2)
      end do
!
      allocate(de_rj(sph_rj%nnod_rj))
      allocate(dre_rj(sph_rj%nnod_rj))
      allocate(d2re_rj(sph_rj%nnod_rj))
      allocate(d3re_rj(sph_rj%nnod_rj))
      do inod = 1, sph_rj%nnod_rj
        j = mod((inod-1),sph_rj%nidx_rj(2)) + 1
        k = 1 + (inod- j) / sph_rj%nidx_rj(2)
!
        de_rj(inod) =   r_ele(k)**j
        dre_rj(inod) =  dble(j) * r_ele(k)**(j-1)
        d2re_rj(inod) = dble(j*(j-1)) * r_ele(k)**(j-2)
        d3re_rj(inod) = dble(j*(j-1)*(j-2)) * r_ele(k)**(j-3)
      end do
!
!
      allocate(dfdr_nod(sph_rj%nnod_rj))
      call cal_second_fdm_node(ione, kr_in, kr_out, sph_rj,             &
     &                         r_2nd, d_rj, dfdr_nod)
      write(*,*) '1st derivative'
      do j = 1, sph_rj%nidx_rj(2)
       do k = kr_in, kr_out
        inod = j + (k-1) * sph_rj%nidx_rj(2)
        r =  sph_rj%radius_1d_rj_r(k)
        write(*,*) j, r, k, (dfdr_nod(inod) - dr_rj(inod)),             &
     &            dfdr_nod(inod), dr_rj(inod)
       end do
      end do
!
      allocate(d2fdr2_nod(sph_rj%nnod_rj))
      call cal_second_fdm_node(itwo, kr_in, kr_out, sph_rj,             &
     &                         r_2nd, d_rj, d2fdr2_nod)
      write(*,*) '2nd derivative'
      do j = 1, sph_rj%nidx_rj(2)
       do k = kr_in, kr_out
        inod = j + (k-1) * sph_rj%nidx_rj(2)
        r =  sph_rj%radius_1d_rj_r(k)
        write(*,*) j, r, k, (d2fdr2_nod(inod) - d2r_rj(inod)),          &
     &            d2fdr2_nod(inod), d2r_rj(inod)
       end do
      end do
!
!
      allocate(d_ele(sph_rj%nnod_rj))
      call cal_third_fdm_node_to_ele(izero, kr_in, kr_out, sph_rj,    &
     &                               r_n2e_3rd, d_rj, d_ele)
      write(*,*) 'Interpolation to element'
      do j = 1, sph_rj%nidx_rj(2)
       do k = kr_in, kr_out
        inod = j + (k-1) * sph_rj%nidx_rj(2)
        r = r_ele(k)
        write(*,*) j, r, k, (d_ele(inod) - de_rj(inod)),                &
     &            d_ele(inod), de_rj(inod)
       end do
      end do
!
      allocate(dfdr_ele(sph_rj%nnod_rj))
      call cal_third_fdm_node_to_ele(ione, kr_in, kr_out, sph_rj,     &
     &                               r_n2e_3rd, d_rj, dfdr_ele)
      write(*,*) '1st derivative from node to element'
      do j = 1, sph_rj%nidx_rj(2)
       do k = kr_in, kr_out
        inod = j + (k-1) * sph_rj%nidx_rj(2)
        r = r_ele(k)
        write(*,*) j, r, k, (dfdr_ele(inod) - dre_rj(inod)),            &
     &            dfdr_ele(inod), dre_rj(inod)
       end do
      end do
!
      allocate(d2fdr2_ele(sph_rj%nnod_rj))
      call cal_third_fdm_node_to_ele(itwo, kr_in, kr_out, sph_rj,     &
     &                               r_n2e_3rd, d_rj, d2fdr2_ele)
      write(*,*) '2nd derivative from node to element'
      do j = 1, sph_rj%nidx_rj(2)
       do k = kr_in, kr_out
        inod = j + (k-1) * sph_rj%nidx_rj(2)
        r = r_ele(k)
        write(*,*) j, r, k, (d2fdr2_ele(inod) - d2re_rj(inod)),         &
     &            d2fdr2_ele(inod), d2re_rj(inod)
       end do
      end do
!
      allocate(d3fdr3_ele(sph_rj%nnod_rj))
      call cal_third_fdm_node_to_ele(ithree, kr_in, kr_out, sph_rj,   &
     &                               r_n2e_3rd, d_rj, d3fdr3_ele)
      write(*,*) '3rd derivative from node to element'
      do j = 1, sph_rj%nidx_rj(2)
       do k = kr_in, kr_out
        inod = j + (k-1) * sph_rj%nidx_rj(2)
        r = r_ele(k)
        write(*,*) j, r, k, (d3fdr3_ele(inod) - d3re_rj(inod)),         &
     &            d3fdr3_ele(inod), d3re_rj(inod)
       end do
      end do
!
!
      allocate(d_e2n(sph_rj%nnod_rj))
      call cal_first_fdm_ele_to_node(izero, kr_in, kr_out, sph_rj,    &
     &                               r_e2n_1st, de_rj, d_e2n)
      write(*,*) 'Interpolation from element to node'
      do j = 1, sph_rj%nidx_rj(2)
       do k = kr_in, kr_out
        inod = j + (k-1) * sph_rj%nidx_rj(2)
        r =  sph_rj%radius_1d_rj_r(k)
        write(*,*) j, r, k, (d_e2n(inod) - d_rj(inod)),                 &
     &            d_e2n(inod), d_rj(inod)
       end do
      end do
!
      allocate(dfdr_e2n(sph_rj%nnod_rj))
      call cal_first_fdm_ele_to_node(ione, kr_in, kr_out, sph_rj,     &
     &                               r_e2n_1st, de_rj, dfdr_e2n)
      write(*,*) '1st derivative from element to node'
      do j = 1, sph_rj%nidx_rj(2)
       do k = kr_in, kr_out
        inod = j + (k-1) * sph_rj%nidx_rj(2)
        r =  sph_rj%radius_1d_rj_r(k)
        write(*,*) j, r, k, (dfdr_e2n(inod) - dr_rj(inod)),             &
     &            dfdr_e2n(inod), dr_rj(inod)
       end do
      end do
!
      end subroutine test_radial_FDM
!
!  -------------------------------------------------------------------
!
      end program FDM_matrices_check
