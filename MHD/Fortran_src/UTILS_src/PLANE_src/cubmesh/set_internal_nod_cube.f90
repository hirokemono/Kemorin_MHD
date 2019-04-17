!set_internal_nod_cube.f90
!     module set_internal_nod_cube
!
!     Written by H. Matsui
!     modified by H. Matsui on Aug., 2007
!
!!      subroutine set_internal_node                                    &
!!     &         (c_size, c_vert, nb_rng, sl_rng, loc_id, inod)
!!      subroutine set_internal_edge(c_size, c_vert, nb_rng, sl_rng,    &
!!     &          kpe, inp, jnp, knp, nd, loc_id, inod)
!!        type(vertical_position_cube), intent(in) :: c_vert
!!        type(neib_range_cube), intent(in) :: nb_rng
!!        type(slleve_range), intent(in) :: sl_rng
!!        type(local_node_id_cube), intent(inout) :: loc_id
!
      module set_internal_nod_cube
!
      use m_precision
!
      use t_size_of_cube
      use t_neib_range_cube
      use t_sleeve_cube
      use t_cube_position
      use t_local_node_id_cube
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
      subroutine set_internal_node                                      &
     &         (c_size, c_vert, nb_rng, sl_rng, loc_id, inod)
!
      type(size_of_cube), intent(in) :: c_size
      type(vertical_position_cube), intent(in) :: c_vert
      type(neib_range_cube), intent(in) :: nb_rng
      type(slleve_range), intent(in) :: sl_rng
!
      type(local_node_id_cube), intent(inout) :: loc_id
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

            loc_id%node_id_lc(i,j,k) =  inod
            node_id_gl        = (nb_rng%ioff + i  )                     &
     &                         + (nb_rng%joff + j-1) * c_size%nx_all    &
     &                         + (nb_rng%koff + k-1) * c_size%nx_all    &
     &                          * c_size%ny_all

            x = nb_rng%xoff + (i-1) * c_size%xsize / (c_size%nx_all)
            y = nb_rng%yoff + (j-1) * c_size%ysize / (c_size%ny_all)
            z = c_vert%zz(nb_rng%koff + k)

            write(l_out,'(i15,3(1pe21.11))')  node_id_gl, x, y, z
          enddo
        enddo
      enddo
!
      end subroutine set_internal_node
!
! ----------------------------------------------------------------------
!
      subroutine set_internal_edge(c_size, c_vert, nb_rng, sl_rng,      &
     &          kpe, inp, jnp, knp, nd, loc_id, inod)
!
      type(size_of_cube), intent(in) :: c_size
      type(vertical_position_cube), intent(in) :: c_vert
      type(neib_range_cube), intent(in) :: nb_rng
      type(slleve_range), intent(in) :: sl_rng
      integer (kind = kint), intent(in) :: kpe
      integer (kind = kint), intent(in) :: nd
      integer (kind = kint), intent(in) :: inp, jnp, knp
!
      integer (kind = kint), intent(inout) :: inod
      type(local_node_id_cube), intent(inout) :: loc_id
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
      if(nd.eq.1 .and. inp.gt.0) ie1 = ie1-1
      if(nd.eq.2 .and. jnp.gt.0) je1 = je1-1
      if(nd.eq.3 .and. knp.gt.0) ke1 = ke1-1
      if(nd.eq.3 .and. kpe.eq.c_size%ndz .and. knp.eq.0) ke1 = ke1-1
      do k = sl_rng%ks, ke1
       do j = sl_rng%js, je1
        do i = sl_rng%is, ie1

         inod = inod + 1

         loc_id%edge_id_lc(i,j,k,nd) =  inod
         node_id_gl        =  nd * c_size%nod_gltot                     &
                             + (nb_rng%ioff + i  )                      &
     &                       + (nb_rng%joff + j-1) * c_size%nx_all      &
     &                       + (nb_rng%koff + k-1) * c_size%nx_all      &
     &                        * c_size%ny_all 

         if (nd .eq. 1) then
          x = nb_rng%xoff + (i-1) * c_size%xsize / dble(c_size%nx_all)  &
     &                    + half * c_size%xsize / dble(c_size%nx_all)
          y = nb_rng%yoff + (j-1) * c_size%ysize / dble(c_size%ny_all)
          z = c_vert%zz(nb_rng%koff + k)
         else if (nd .eq. 2) then
          x = nb_rng%xoff + (i-1) * c_size%xsize / dble(c_size%nx_all)
          y = nb_rng%yoff + (j-1) * c_size%ysize / dble(c_size%ny_all)  &
     &                     + half * c_size%ysize / dble(c_size%ny_all)
          z = c_vert%zz(nb_rng%koff + k)
         else if (nd .eq. 3) then
          x = nb_rng%xoff + (i-1) * c_size%xsize / dble(c_size%nx_all)
          y = nb_rng%yoff + (j-1) * c_size%ysize / dble(c_size%ny_all)
          z = c_vert%zz_edge(nb_rng%koff + k)
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
