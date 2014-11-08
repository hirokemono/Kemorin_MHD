!set_center_cube_node.f90
!      module set_center_cube_node
!
!      Written by H. Matsui on Apr., 2006
!
!      subroutine set_center_cube(inod, ifile, ifile_q, num_h, num_v,   &
!     &          x_node, v_node)
!      subroutine set_coarse_center_cube(inod, id, id_f2c,              &
!     &          nskip_s, nl_s, num_h, num_v, x_node, v_node)
!      subroutine set_center_square(inod, ifile, num_h, x_node)
!
      module set_center_cube_node
!
      use m_precision
!
      implicit none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine set_center_cube(inod, ifile, ifile_q, num_h, num_v,    &
     &          x_node, v_node)
!
      integer(kind = kint), intent(in) ::  ifile, ifile_q
      integer(kind = kint), intent(in) ::  num_h, num_v
      real(kind = kreal), intent(in) :: x_node(num_h+1)
      real(kind = kreal), intent(in) :: v_node(num_v+1)
      integer(kind = kint), intent(inout) ::  inod
!
      integer(kind = kreal) :: ix, iy, iz
!
!
      do iz = 1, (num_v-1)
        do iy = 1, (num_h-1)
          do ix = 1, (num_h-1)
!
            inod = inod + 1
            write(ifile,'(i15,1p3E25.15e3)') inod,                      &
     &               x_node(ix+1), x_node(iy+1), v_node(iz+1)
            if(ifile_q .gt. 0) then
              write(ifile_q,'(i15,1p3E25.15e3)') inod,                  &
     &               x_node(ix+1), x_node(iy+1), v_node(iz+1)
            end if
!
          end do
        end do
      end do
!
      end subroutine set_center_cube
!
!   --------------------------------------------------------------------
!
      subroutine set_coarse_center_cube(inod, id, id_f2c,               &
     &          nskip_s, nl_s, num_h, num_v, x_node, v_node)
!
      use m_constants
!
      integer(kind = kint), intent(in) ::  id, id_f2c
      integer(kind = kint), intent(in) ::  num_h, num_v
      integer(kind = kint), intent(in) ::  nskip_s, nl_s
      real(kind = kreal), intent(in) :: x_node(num_h+1)
      real(kind = kreal), intent(in) :: v_node(num_v+1)
!
      integer(kind = kint), intent(inout) ::  inod
!
      integer(kind = kreal) :: jnod, knod
      integer(kind = kreal) :: num_hc, num_vc, num_hfc
      integer(kind = kreal) :: ix, iy, iz
!
      num_hc =   num_h/nskip_s
      num_vc =   num_v/nskip_s
      num_hfc =  nl_s * num_hc
!
      do iz = 1, num_vc-1
        do iy = 1, num_hc-1
          do ix = 1, num_hc-1
            inod =  (num_h-1)**2 * (iz*nskip_s-1)                       &
                  + (num_h-1)*(iy*nskip_s-1) + ix*nskip_s
            jnod =  (num_hc-1)**2 * (iz-1)                              &
     &            + (num_hc-1)*(iy-1) + ix
            knod =  (num_hfc-1)**2 * (iz*nl_s-1)                        &
     &            + (num_hfc-1)*(iy*nl_s-1) + ix*nl_s
!
            write(id,'(i15,1p3E25.15e3)') jnod, x_node(ix*nskip_s+1),   &
     &            x_node(iy*nskip_s+1), v_node(iz*nskip_s+1)
            write(id_f2c,'(6i10)') izero, jnod,                         &
     &            izero, knod, izero, inod
!
          end do
        end do
      end do
!
      end subroutine set_coarse_center_cube
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine set_center_square(inod, ifile, ifile_q, num_h, x_node)
!
      use m_constants
!
      integer(kind = kint), intent(in) ::  ifile, ifile_q
      integer(kind = kint), intent(in) ::  num_h
      real(kind = kreal), intent(in) :: x_node(num_h+1)
      integer(kind = kint), intent(inout) ::  inod
!
      integer(kind = kreal) :: ix, iy
!
!
      do iy = 1, num_h-1
        do ix = 1, num_h-1
          inod = inod + 1
          write(ifile,'(i15,1p3E25.15e3)') inod,                        &
     &                x_node(ix+1), x_node(iy+1), zero
          if(ifile_q .gt. 0) then
            write(ifile_q,'(i15,1p3E25.15e3)') inod,                    &
     &                x_node(ix+1), x_node(iy+1), zero
            end if
        end do
      end do
!
      end subroutine set_center_square
!
!   --------------------------------------------------------------------
!
      end module set_center_cube_node
