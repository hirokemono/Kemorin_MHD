!peri_cube_shell_position.f90
!      module peri_cube_shell_position
!
!        programmed by H.Matsui on Apr., 2006
!
!      subroutine allocate_prei_cube_surf_tmp
!      subroutine deallocate_prei_cube_surf_tmp
!
!      subroutine cover_peri_cube(inod, ifile, ifile_q)
!
!      subroutine cover_coarse_cube(is_level, ifile, id_f2c)
!
      module peri_cube_shell_position
!
      use m_precision
!
      use m_constants
      use m_cubed_sph_mesh
      use m_cubed_sph_surf_mesh
      use m_cubed_sph_radius
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
      subroutine allocate_prei_cube_surf_tmp
!
      integer(kind = kint) :: num
!
      num = numnod_sf+numedge_sf
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
      subroutine allocate_coarse_cube_surf_tmp
!
      use m_numref_cubed_sph
!
      integer(kind = kint) :: num
!
      num = inod_stack_sf(max_coarse_level)
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
      subroutine cover_peri_cube(inod, ifile, ifile_q)
!
      use coordinate_converter
!
      integer(kind = kint), intent(in) :: ifile, ifile_q
      integer(kind = kint), intent(inout) :: inod
!
      integer(kind = kint) :: inod0
!
!
      call position_2_xyz(numnod_sf,                                    &
     &      r_surf(1), theta_surf(1), phi_surf(1), x(1), y(1), z(1))
!
        do inod0 = 1, numnod_sf
          inod = inod + 1
          write (ifile,'(i10,1p3E25.15e3)')                             &
     &          inod, x(inod0), y(inod0), z(inod0)
          if (ifile_q .gt. 0) then
            write (ifile_q,'(i10,1p3E25.15e3)')                         &
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
      subroutine cover_coarse_cube(is_level, ifile, id_f2c)
!
      use m_numref_cubed_sph
      use coordinate_converter
!
      integer(kind = kint), intent(in) :: ifile, id_f2c, is_level
!
      integer(kind = kint) :: inod0, inod, jnod, knod, num
      integer(kind = kint) :: ist, ied, i_sf, j_sf, k_sf
!
!
        ist = inod_stack_sf(is_level-1)+1
        ied = inod_stack_sf(is_level)
        num = ied - ist + 1
!
        do inod0 = ist, ied
          i_sf = inod_2_org(inod0)
          r(inod0) = r_surf(i_sf)
          t(inod0) = theta_surf(i_sf)
          p(inod0) = phi_surf(i_sf)
        end do
!
        call position_2_xyz(num, r(ist), t(ist), p(ist),                &
     &      x(ist), y(ist), z(ist))
!
        do inod0 = ist, ied
          i_sf = inod_2_org(inod0)
          j_sf = inod0 - inod_stack_sf(is_level-1)
          k_sf = inod_2_next(inod0)
!
          inod = i_sf + numnod_cube + numnod_sf*(1-1)
          jnod = j_sf + nnod_cube_c + nnod_sf_c*((1-1)/nskip_r)
          knod = k_sf + nnod_cube_fc + nnod_sf_fc*((1-1)/nskip_fr)
!
          write (ifile,'(i10,1p3E25.15e3)')                             &
     &         jnod, x(inod0), y(inod0), z(inod0)
          write (id_f2c,'(6i10)') izero, jnod, izero, knod, izero, inod
!
        end do
!
      end subroutine cover_coarse_cube
!
!   --------------------------------------------------------------------
!
      end module peri_cube_shell_position
