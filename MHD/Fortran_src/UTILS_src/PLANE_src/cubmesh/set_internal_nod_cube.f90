!set_internal_nod_cube.f90
!     module set_internal_nod_cube
!
!     Written by H. Matsui
!     modified by H. Matsui on Aug., 2007
!
!!      subroutine set_internal_node(nb_rng, sl_rng, inod)
!!      subroutine set_internal_edge                                    &
!!     &         (nb_rng, sl_rng, kpe, inp, jnp, knp, inod, nd)
!!        type(neib_range_cube), intent(in) :: nb_rng
!!        type(slleve_range), intent(in) :: sl_rng
!
      module set_internal_nod_cube
!
      use m_precision
!
      use t_neib_range_cube
      use t_sleeve_cube
      use m_size_4_plane
      use m_size_of_cube
      use m_cube_position
      use m_local_node_id_cube
      use m_cube_files_data
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_internal_node(nb_rng, sl_rng, inod)
!
      type(neib_range_cube), intent(in) :: nb_rng
      type(slleve_range), intent(in) :: sl_rng
      integer (kind = kint), intent(inout) :: inod
!
      integer (kind= kint) :: node_id_gl
      integer (kind= kint) :: i, j, k
      real (kind= kreal) :: x, y, z
!
! *****   set position of internal node
!
      do k = sl_rng%ks, sl_rng%ke
        do j = sl_rng%js, sl_rng%je
          do i = sl_rng%is, sl_rng%ie
            inod = inod + 1

            node_id_lc(i,j,k) =  inod
            node_id_gl        = (nb_rng%ioff + i  )                     &
     &                         + (nb_rng%joff + j-1)*nx_all             &
     &                         + (nb_rng%koff + k-1)*nx_all*ny_all

            x = xoff + (i-1) * xsize/(nx_all)
            y = yoff + (j-1) * ysize/(ny_all)
            z = zz(nb_rng%koff + k)

            write(l_out,'(i15,3(1pe21.11))')                      &
     &              node_id_gl, x, y, z
          enddo
        enddo
      enddo
!
      end subroutine set_internal_node
!
! ----------------------------------------------------------------------
!
      subroutine set_internal_edge                                      &
     &         (nb_rng, sl_rng, kpe, inp, jnp, knp, inod, nd)
!
      type(neib_range_cube), intent(in) :: nb_rng
      type(slleve_range), intent(in) :: sl_rng
      integer (kind = kint), intent(in) :: kpe
      integer (kind = kint), intent(in) :: nd
      integer (kind = kint), intent(in) :: inp, jnp, knp
      integer (kind = kint), intent(inout) :: inod
!
      integer (kind= kint) :: node_id_gl
      integer (kind= kint) :: i, j, k
      integer (kind= kint) :: ie1, je1, ke1
      real(kind = kreal) :: x, y, z
      real(kind = kreal), parameter   ::  half = 0.5d0
!
!
!       for edge on y=const and z=const
!
      ie1 = sl_rng%ie
      je1 = sl_rng%je
      ke1 = sl_rng%ke
      if (nd.eq.1 .and. inp.gt.0) ie1 = ie1-1
      if (nd.eq.2 .and. jnp.gt.0) je1 = je1-1
      if (nd.eq.3 .and. knp.gt.0) ke1 = ke1-1
      if (nd.eq.3 .and. kpe.eq.ndz .and. knp.eq.0) ke1 = ke1-1
      do k = sl_rng%ks, ke1
       do j = sl_rng%js, je1
        do i = sl_rng%is, ie1

         inod = inod + 1

         edge_id_lc(i,j,k,nd) =  inod
         node_id_gl        =  nd*nod_gltot                              &
                             + (nb_rng%ioff + i  )                      &
     &                       + (nb_rng%joff + j-1)*nx_all               &
     &                       + (nb_rng%koff + k-1)*nx_all*ny_all 

         if (nd .eq. 1) then
          x = xoff + (i-1) * xsize/(nx_all) + half*xsize/(nx_all)
          y = yoff + (j-1) * ysize/(ny_all)
          z = zz(nb_rng%koff + k)
         else if (nd .eq. 2) then
          x = xoff + (i-1) * xsize/(nx_all)
          y = yoff + (j-1) * ysize/(ny_all) + half*ysize/(ny_all)
          z = zz(nb_rng%koff + k)
         else if (nd .eq. 3) then
          x = xoff + (i-1) * xsize/(nx_all)
          y = yoff + (j-1) * ysize/(ny_all)
          z = zz_edge(nb_rng%koff + k)
         end if

         write(l_out,'(i15,3(1pe21.11))') node_id_gl, x, y, z
        end do
       end do
      end do
!
      end subroutine set_internal_edge
!
! ----------------------------------------------------------------------
!
      end module set_internal_nod_cube
