!peri_cube_shell_position.f90
!      module peri_cube_shell_position
!
!        programmed by H.Matsui on Apr., 2006
!
!!     subroutine allocate_prei_cube_surf_tmp(c_sphere)
!!     subroutine allocate_coarse_cube_surf_tmp(c_sphere)
!!        type(cubed_sph_surf_mesh), intent(in) :: c_sphere
!!      subroutine deallocate_prei_cube_surf_tmp
!!
!!      subroutine cover_peri_cube(ifile, ifile_q, c_sphere, inod)
!!        type(cubed_sph_surf_mesh), intent(in) :: c_sphere
!!
!!      subroutine cover_coarse_cube(is_level, ifile, id_f2c, c_sphere)
!!        type(cubed_sph_surf_mesh), intent(in) :: c_sphere
!
      module peri_cube_shell_position
!
      use m_precision
!
      use m_constants
      use t_cubed_sph_surf_mesh
!
      implicit  none
!
      real(kind= kreal), allocatable :: x(:), y(:), z(:)
      real(kind= kreal), allocatable :: r(:), t(:), p(:)
      real(kind= kreal), allocatable :: ratio(:)
      private :: x, y, z, r, t, p, ratio
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine allocate_prei_cube_surf_tmp(c_sphere)
!
      type(cubed_sph_surf_mesh), intent(in) :: c_sphere
      integer(kind = kint) :: num
!
      num = c_sphere%numnod_sf + c_sphere%numedge_sf
      allocate( x(num), y(num), z(num) )
      allocate( r(num), t(num), p(num) )
      allocate( ratio(num))
      x = 0.0d0
      y = 0.0d0
      z = 0.0d0
      r = 0.0d0
      t = 0.0d0
      p = 0.0d0
      ratio =  0.0d0
!
      end subroutine allocate_prei_cube_surf_tmp
!
!   --------------------------------------------------------------------
!
      subroutine allocate_coarse_cube_surf_tmp(c_sphere)
!
      use m_numref_cubed_sph
!
      type(cubed_sph_surf_mesh), intent(in) :: c_sphere
      integer(kind = kint) :: num
!
      num = c_sphere%inod_stack_sf(max_coarse_level)
      allocate( x(num), y(num), z(num) )
      allocate( r(num), t(num), p(num) )
      allocate( ratio(num) )
      x = 0.0d0
      y = 0.0d0
      z = 0.0d0
      r = 0.0d0
      t = 0.0d0
      p = 0.0d0
      ratio =    0.0d0
!
      end subroutine allocate_coarse_cube_surf_tmp
!
!   --------------------------------------------------------------------
!
      subroutine deallocate_prei_cube_surf_tmp
!
      deallocate( x, y, z, r, t, p, ratio)
!
      end subroutine deallocate_prei_cube_surf_tmp
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine cover_peri_cube(ifile, ifile_q, c_sphere, inod)
!
      use coordinate_converter
!
      integer(kind = kint), intent(in) :: ifile, ifile_q
      type(cubed_sph_surf_mesh), intent(in) :: c_sphere
!
      integer(kind = kint), intent(inout) :: inod
!
      integer(kind = kint) :: inod0
!
!
      call position_2_xyz(c_sphere%numnod_sf, c_sphere%r_csph(1),       &
     &    c_sphere%theta_csph(1), c_sphere%phi_csph(1),                 &
     &    x(1), y(1), z(1))
!
        do inod0 = 1, c_sphere%numnod_sf
          inod = inod + 1
          write (ifile,'(i16,1p3E25.15e3)')                             &
     &          inod, x(inod0), y(inod0), z(inod0)
          if (ifile_q .gt. 0) then
            write (ifile_q,'(i16,1p3E25.15e3)')                         &
     &          inod, x(inod0), y(inod0), z(inod0)
          end if
!
      end do
!
      end subroutine cover_peri_cube
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine cover_coarse_cube(is_level, ifile, id_f2c, c_sphere)
!
      use m_numref_cubed_sph
      use coordinate_converter
!
      integer(kind = kint), intent(in) :: ifile, id_f2c, is_level
      type(cubed_sph_surf_mesh), intent(in) :: c_sphere
!
      integer(kind = kint) :: inod0, inod, jnod, knod, num
      integer(kind = kint) :: ist, ied, i_sf, j_sf, k_sf
!
!
        ist = c_sphere%inod_stack_sf(is_level-1)+1
        ied = c_sphere%inod_stack_sf(is_level)
        num = ied - ist + 1
!
        do inod0 = ist, ied
          i_sf = c_sphere%inod_2_org(inod0)
          r(inod0) = c_sphere%r_csph(i_sf)
          t(inod0) = c_sphere%theta_csph(i_sf)
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
     &          + c_sphere%numnod_sf * (1-1)
          jnod = j_sf + nnod_cube_c + nnod_sf_c*((1-1)/nskip_r)
          knod = k_sf + nnod_cube_fc + nnod_sf_fc*((1-1)/nskip_fr)
!
          write (ifile,'(i16,1p3E25.15e3)')                             &
     &         jnod, x(inod0), y(inod0), z(inod0)
          write (id_f2c,'(6i16)') izero, jnod, izero, knod, izero, inod
!
        end do
!
      end subroutine cover_coarse_cube
!
!   --------------------------------------------------------------------
!
      end module peri_cube_shell_position
