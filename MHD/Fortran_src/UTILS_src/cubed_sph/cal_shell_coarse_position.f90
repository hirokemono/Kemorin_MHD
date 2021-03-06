!cal_shell_coarse_position.f90
!      module cal_shell_coarse_position
!
!        programmed by H.Matsui on Apr., 2006
!
!!      subroutine allocate_coarse_cube_sph_posi                        &
!!     &         (max_coarse_level, c_sphere)
!!      subroutine deallocate_coarse_cube_sph_posi
!!
!!      subroutine projection_coarse(is_level, ifile, id_f2c,           &
!!     &          nnod_cube_c, nnod_sf_c, nnod_cube_fc, nnod_sf_fc,     &
!!     &          nskip_r, nskip_fr, num_h, num_v, rprm_csph, c_sphere)
!!      subroutine adjust_to_coarse_shell(is_level, ifile, id_f2c,      &
!!     &          nnod_cube_c, nnod_sf_c, nnod_cube_fc, nnod_sf_fc,     &
!!     &          nskip_r, nskip_fr, num_h, num_v, rprm_csph, c_sphere)
!!        type(cubed_sph_radius), intent(in) :: rprm_csph
!!        type(cubed_sph_surf_mesh), intent(in) :: c_sphere
!
      module cal_shell_coarse_position
!
      use m_precision
      use m_constants
!
      use t_cubed_sph_surf_mesh
      use t_cubed_sph_radius
!
      implicit  none
!
      real(kind= kreal), allocatable :: x(:), y(:), z(:)
      real(kind= kreal), allocatable :: r(:), t(:), p(:)
      real(kind= kreal), allocatable :: ratio(:), theta_mod(:)
      private :: x, y, z, r, t, p, ratio, theta_mod

!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine allocate_coarse_cube_sph_posi                          &
     &         (max_coarse_level, c_sphere)
!
      integer(kind = kint), intent(in) :: max_coarse_level
      type(cubed_sph_surf_mesh), intent(in) :: c_sphere
      integer(kind = kint) :: num
!
      num = c_sphere%inod_stack_sf(max_coarse_level)
      allocate( x(num), y(num), z(num) )
      allocate( r(num), t(num), p(num) )
      allocate( ratio(num) )
      allocate( theta_mod(c_sphere%numnod_sf))
      x = 0.0d0
      y = 0.0d0
      z = 0.0d0
      r = 0.0d0
      t = 0.0d0
      p = 0.0d0
      ratio =    0.0d0
!
      theta_mod = 0.0d0
!
      end subroutine allocate_coarse_cube_sph_posi
!
!   --------------------------------------------------------------------
!
      subroutine deallocate_coarse_cube_sph_posi
!
      deallocate( x, y, z, r, t, p, ratio, theta_mod)
!
      end subroutine deallocate_coarse_cube_sph_posi
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine projection_coarse(is_level, ifile, id_f2c,             &
     &          nnod_cube_c, nnod_sf_c, nnod_cube_fc, nnod_sf_fc,       &
     &          nskip_r, nskip_fr, num_h, num_v, rprm_csph, c_sphere)
!
      use coordinate_converter
      use modify_colat_cube_surf
!
      integer(kind = kint), intent(in) :: ifile, id_f2c, is_level
      integer(kind = kint), intent(in) :: nnod_cube_c,  nnod_sf_c
      integer(kind = kint), intent(in) :: nnod_cube_fc, nnod_sf_fc
      integer(kind = kint), intent(in) :: nskip_r, nskip_fr
      integer(kind = kint), intent(in) :: num_h, num_v
      type(cubed_sph_radius), intent(in) :: rprm_csph
      type(cubed_sph_surf_mesh), intent(in) :: c_sphere
!
      integer(kind = kint) :: k, inod0
      integer(kind = kint) :: inod, jnod, knod
      integer(kind = kint) :: ist, ied, kst, i_sf, j_sf, k_sf, num
!
!
      kst = rprm_csph%nr_adj - mod(rprm_csph%nr_adj-1,nskip_r)          &
     &     + nskip_r
!
      do k = kst, rprm_csph%n_shell, nskip_r
        ist = c_sphere%inod_stack_sf(is_level-1)+1
        ied = c_sphere%inod_stack_sf(is_level)
        num = ied - ist + 1
!
        if(rprm_csph%nedge_ref_lat.gt.0 .or. num_h.ne.num_v) then
          call cal_wall_latitude_ratio                                  &
     &       (num_h, num_h, rprm_csph%edge_latitude(k))
          call modify_colat_on_cube_sf                                  &
     &       (c_sphere%numnod_sf, num_h, num_v,                         &
     &        c_sphere%theta_csph(1), theta_mod(1))
        else
          theta_mod(1:c_sphere%numnod_sf)                               &
     &         = c_sphere%theta_csph(1:c_sphere%numnod_sf)
        end if
!
        do inod0 = ist, ied
          i_sf = c_sphere%inod_2_org(inod0)
          r(inod0) = rprm_csph%r_nod(k)
          t(inod0) = theta_mod(i_sf)
          p(inod0) = c_sphere%phi_csph(i_sf)
        end do
!
        call position_2_xyz(num, r(ist), t(ist), p(ist),                &
     &      x(ist), y(ist), z(ist))
!
        do inod0 = ist, ied
          i_sf = c_sphere%inod_2_org(inod0)
          j_sf = inod0 - c_sphere%inod_stack_sf(is_level-1)
          k_sf = c_sphere%inod_2_next(inod0)
!
          inod = i_sf + c_sphere%numnod_cube                            &
     &                + c_sphere%numnod_sf*(k-1)
          jnod = j_sf + nnod_cube_c + nnod_sf_c*((k-1)/nskip_r)
          knod = k_sf + nnod_cube_fc + nnod_sf_fc*((k-1)/nskip_fr)
!
          write (ifile,'(i16,1p3E25.15e3)')                             &
     &         jnod, x(inod0), y(inod0), z(inod0)
          write (id_f2c,'(6i16)') izero, jnod, izero, knod, izero, inod
!
        end do
      end do
!
      end subroutine projection_coarse
!
!   --------------------------------------------------------------------
!
      subroutine adjust_to_coarse_shell(is_level, ifile, id_f2c,        &
     &          nnod_cube_c, nnod_sf_c, nnod_cube_fc, nnod_sf_fc,       &
     &          nskip_r, nskip_fr, num_h, num_v, rprm_csph, c_sphere)
!
      use coordinate_converter
      use modify_colat_cube_surf
!
      integer(kind = kint), intent(in) :: ifile, id_f2c, is_level
      integer(kind = kint), intent(in) :: nnod_cube_c,  nnod_sf_c
      integer(kind = kint), intent(in) :: nnod_cube_fc, nnod_sf_fc
      integer(kind = kint), intent(in) :: nskip_r, nskip_fr
      integer(kind = kint), intent(in) :: num_h, num_v
      type(cubed_sph_radius), intent(in) :: rprm_csph
      type(cubed_sph_surf_mesh), intent(in) :: c_sphere
!
      integer(kind = kint) :: k, inod0
      integer(kind = kint) :: inod, jnod, knod, num
      integer(kind = kint) :: ist, ied, i_sf, j_sf, k_sf
!
!
      do k = 1, rprm_csph%nr_adj, nskip_r
        ist = c_sphere%inod_stack_sf(is_level-1)+1
        ied = c_sphere%inod_stack_sf(is_level)
        num = ied - ist + 1
!
        if(rprm_csph%nedge_ref_lat.gt.0 .or. num_h.ne.num_v) then
          call cal_wall_latitude_ratio                                  &
     &       (num_h, num_h, rprm_csph%edge_latitude(k))
          call modify_colat_on_cube_sf                                  &
     &       (c_sphere%numnod_sf, num_h, num_v,                         &
     &        c_sphere%theta_csph(1), theta_mod(1))
        else
          theta_mod(1:c_sphere%numnod_sf)                               &
     &       = c_sphere%theta_csph(1:c_sphere%numnod_sf)
        end if
!
        do inod0 = ist, ied
          i_sf = c_sphere%inod_2_org(inod0)
          ratio(inod0) = (dble(rprm_csph%nr_adj - k)                    &
     &            + dble(k-1)*rprm_csph%r_nod(1)/c_sphere%r_csph(i_sf)) &
     &             * rprm_csph%r_nod(k)                                 &
     &             / ( dble(rprm_csph%nr_adj - 1)*rprm_csph%r_nod(1) )
!
          r(inod0) = c_sphere%r_csph(i_sf) * ratio(inod0)
          t(inod0) = theta_mod(i_sf)
          p(inod0) = c_sphere%phi_csph(i_sf)
        end do
!
        call position_2_xyz(num, r(ist), t(ist), p(ist),                &
     &      x(ist), y(ist), z(ist))
!
        do inod0 = ist, ied
          i_sf = c_sphere%inod_2_org(inod0)
          j_sf = inod0 - c_sphere%inod_stack_sf(is_level-1)
          k_sf = c_sphere%inod_2_next(inod0)
!
          inod = i_sf + c_sphere%numnod_cube                            &
     &                + c_sphere%numnod_sf*(k-1)
          jnod = j_sf + nnod_cube_c + nnod_sf_c*((k-1)/nskip_r)
          knod = k_sf + nnod_cube_fc + nnod_sf_fc*((k-1)/nskip_fr)
!
          write (ifile,'(i16,1p3E25.15e3)')                             &
     &         jnod, x(inod0), y(inod0), z(inod0)
          write (id_f2c,'(6i16)') izero, jnod, izero, knod, izero, inod
!
        end do
      end do
!
      end subroutine adjust_to_coarse_shell
!
!   --------------------------------------------------------------------
!
      end module cal_shell_coarse_position
