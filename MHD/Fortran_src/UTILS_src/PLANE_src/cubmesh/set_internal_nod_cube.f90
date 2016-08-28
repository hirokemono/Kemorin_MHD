!set_internal_nod_cube.f90
!     module set_internal_nod_cube
!
!     Written by H. Matsui
!     modified by H. Matsui on Aug., 2007
!
!      subroutine set_internal_node(inod)
!      subroutine set_internal_edge(kpe, inp, jnp, knp, inod, nd)
!
      module set_internal_nod_cube
!
      use m_precision
!
      use m_size_4_plane
      use m_size_of_cube
      use m_offset_size_cube
      use m_sleeve_cube
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
      subroutine set_internal_node(inod)
!
      integer (kind = kint), intent(inout) :: inod
!
      integer (kind= kint) :: node_id_gl
      integer (kind= kint) :: i, j, k
      real (kind= kreal) :: x, y, z
!
! *****   set position of internal node
!
            do k=ks, ke
              do j=js, je
                do i=is, ie

                  inod = inod + 1

                  node_id_lc(i,j,k) =  inod
                  node_id_gl        = (ioff+i  ) +                      &
     &                                (joff+j-1)*nx_all +               &
     &                                (koff+k-1)*nx_all*ny_all 

                  x = xoff + (i-1)*xsize/(nx_all)
                  y = yoff + (j-1)*ysize/(ny_all)
                  z = zz(koff+k)

                  write(l_out,'(i15,3(1pe21.11))')                      &
     &                    node_id_gl, x, y, z
                enddo
              enddo
            enddo
!
      end subroutine set_internal_node
!
! ----------------------------------------------------------------------
!
      subroutine set_internal_edge(kpe, inp, jnp, knp, inod, nd)
!
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
      ie1 = ie
      je1 = je
      ke1 = ke
      if (nd.eq.1 .and. inp.gt.0) ie1 = ie1-1
      if (nd.eq.2 .and. jnp.gt.0) je1 = je1-1
      if (nd.eq.3 .and. knp.gt.0) ke1 = ke1-1
      if (nd.eq.3 .and. kpe.eq.ndz .and. knp.eq.0) ke1 = ke1-1
      do k=ks,ke1
       do j=js,je1
        do i=is,ie1

         inod = inod + 1

         edge_id_lc(i,j,k,nd) =  inod
         node_id_gl        =  nd*nod_gltot                              &
                             + (ioff+i  )                               &
     &                       + (joff+j-1)*nx_all                        &
     &                       + (koff+k-1)*nx_all*ny_all 

         if (nd .eq. 1) then
          x = xoff + (i-1)*xsize/(nx_all) + half*xsize/(nx_all)
          y = yoff + (j-1)*ysize/(ny_all)
          z = zz(koff+k)
         else if (nd .eq. 2) then
          x = xoff + (i-1)*xsize/(nx_all)
          y = yoff + (j-1)*ysize/(ny_all) + half*ysize/(ny_all)
          z = zz(koff+k)
         else if (nd .eq. 3) then
          x = xoff + (i-1)*xsize/(nx_all)
          y = yoff + (j-1)*ysize/(ny_all)
          z = zz_edge(koff+k)
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
