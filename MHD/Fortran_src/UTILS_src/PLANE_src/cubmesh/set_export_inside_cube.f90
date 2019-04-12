!set_export_inside_cube.f90
!     module set_export_inside_cube
!
!     Written by H. Matsui
!     modified by H. Matsui on Aug., 2007
!
!      subroutine set_export_inside(inod)
!      subroutine set_export_inside_quad(kpe, inod)
!
      module set_export_inside_cube
!
      use m_precision
!
      use m_size_of_cube
      use m_comm_data_cube_kemo
      use m_neighb_range_cube
      use m_sleeve_cube
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
      subroutine set_export_inside(inod)
!
      integer (kind = kint) :: inod
      integer (kind = kint) :: inp, jnp, knp
!
!
            do knp = nb_rng1%knp_st, nb_rng1%knp_end
             do jnp = nb_rng1%jnp_st, nb_rng1%jnp_end
              do inp = nb_rng1%inp_st, nb_rng1%inp_end

               if ((inp==0).and.(jnp==0).and.(knp==0)) cycle

               call set_boundary_size(inp, jnp, knp, nb_rng1)

               neibpetot = neibpetot  + 1

               call set_ex_node(inod)
!               write(*,*) 'inod', inod

              enddo
             enddo
            enddo
!
!
      end subroutine set_export_inside
!
! ----------------------------------------------------------------------
!
      subroutine set_export_inside_quad(kpe, inod)
!
      use set_comm_edge_4_cube
!
      integer (kind = kint) :: kpe
      integer (kind = kint) :: inod
!
      integer (kind = kint) :: inp, jnp, knp
      integer (kind = kint) :: nd
!
!
            do knp = nb_rng1%knp_st, nb_rng1%knp_end
             do jnp = nb_rng1%jnp_st, nb_rng1%jnp_end
              do inp = nb_rng1%inp_st, nb_rng1%inp_end

               if ((inp==0).and.(jnp==0).and.(knp==0)) cycle

               call set_boundary_size(inp, jnp, knp, nb_rng1)

               neibpetot = neibpetot  + 1

               call set_ex_node(inod)
               write(*,*) 'export node 0 to',                           &
     &                   (neibpe(neibpetot)-1), inp, jnp, knp, inod

               nd = 1
               call set_ex_edge(kpe, inp, jnp, knp, inod, nd)
               write(*,*) 'export edge1 0 to',                          &
     &                   (neibpe(neibpetot)-1), inp, jnp, knp, inod

               nd = 2
               call set_ex_edge(kpe, inp, jnp, knp, inod, nd)
               write(*,*) 'export edge2 0 to',                          &
     &                   (neibpe(neibpetot)-1), inp, jnp, knp, inod

               nd = 3
               call set_ex_edge(kpe, inp, jnp, knp, inod, nd)
               write(*,*) 'export edge3 0 to',                          &
     &                   (neibpe(neibpetot)-1), inp, jnp, knp, inod

              enddo
             enddo
            enddo
!
      end subroutine set_export_inside_quad
!
!
! ----------------------------------------------------------------------
!
      end module set_export_inside_cube
