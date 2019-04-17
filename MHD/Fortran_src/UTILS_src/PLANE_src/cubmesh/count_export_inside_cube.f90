!count_export_inside_cube.f90
!     module count_export_inside_cube
!
!     Written by H. Matsui
!     modified by H. Matsui on Aug., 2007
!
!!      subroutine count_export_inside(c_size, nb_rng,                  &
!!     &          num_neib, istack_export, icou, inod)
!!      subroutine count_export_inside_quad(c_size, nb_rng, kpe,        &
!!     &          num_neib, istack_export, icou, inod)
!!        type(size_of_cube), intent(in) :: c_size
!!        type(neib_range_cube), intent(in) :: nb_rng
!
      module count_export_inside_cube
!
      use m_precision
      use m_constants
!
      use t_neib_range_cube
      use t_sleeve_cube
      use t_size_of_cube
      use m_comm_data_cube_kemo
      use set_comm_nod_4_cube
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine count_export_inside(c_size, nb_rng,                    &
     &          num_neib, istack_export, icou, inod)
!
      type(size_of_cube), intent(in) :: c_size
      type(neib_range_cube), intent(in) :: nb_rng
      integer (kind = kint), intent(in) :: num_neib
!
      integer (kind = kint), intent(inout) :: istack_export(0:num_neib)
      integer (kind = kint), intent(inout) :: icou, inod
!
      type(slleve_range) :: sl_rng1
      integer (kind = kint) :: inp, jnp, knp


            do knp = nb_rng%knp_st, nb_rng%knp_end
              do jnp = nb_rng%jnp_st, nb_rng%jnp_end
                do inp = nb_rng%inp_st, nb_rng%inp_end

                  if ((inp==0).and.(jnp==0).and.(knp==0)) cycle

                  call set_boundary_size                                &
     &               (nb_rng, c_size%ndepth, inp, jnp, knp, sl_rng1)

                  icou = icou  + 1
                  call count_node_id(sl_rng1, inod)

                  istack_export(icou) = inod

                enddo
              enddo
            enddo
!
      end subroutine count_export_inside
!
! ----------------------------------------------------------------------
!
      subroutine count_export_inside_quad(c_size, nb_rng, kpe,          &
     &          num_neib, istack_export, icou, inod)
!
      use set_comm_edge_4_cube
!
      type(size_of_cube), intent(in) :: c_size
      type(neib_range_cube), intent(in) :: nb_rng
      integer (kind = kint), intent(in) :: kpe
      integer (kind = kint), intent(in) :: num_neib
!
      integer (kind = kint), intent(inout) :: istack_export(0:num_neib)
      integer (kind = kint), intent(inout) :: icou, inod
!
      type(slleve_range) :: sl_rng1
      integer (kind = kint) :: inp, jnp, knp
!
!
      do knp = nb_rng%knp_st, nb_rng%knp_end
       do jnp = nb_rng%jnp_st, nb_rng%jnp_end
        do inp = nb_rng%inp_st, nb_rng%inp_end

         if ((inp==0).and.(jnp==0).and.(knp==0)) cycle

         call set_boundary_size                                         &
     &      (nb_rng, c_size%ndepth, inp, jnp, knp, sl_rng1)

         icou = icou  + 1

         call count_node_id(sl_rng1, inod)

         call count_ex_edge(sl_rng1, c_size%ndz,                        &
     &       kpe, inp, jnp, knp, inod, ione)
         call count_ex_edge(sl_rng1, c_size%ndz,                        &
     &       kpe, inp, jnp, knp, inod, itwo)
         call count_ex_edge(sl_rng1, c_size%ndz,                        &
     &       kpe, inp, jnp, knp, inod, ithree)

         istack_export(icou) = inod
!
        end do
       end do
      end do
!
      end subroutine count_export_inside_quad
!
! ----------------------------------------------------------------------
!
      end module count_export_inside_cube
