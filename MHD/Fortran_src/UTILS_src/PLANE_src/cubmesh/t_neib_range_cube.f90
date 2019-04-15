!
!     module t_neib_range_cube
!
!     written by Kemorin
!
!!      subroutine set_offset_of_domain(c_size, ipe, jpe, kpe, nb_rng)
!!      subroutine init_node_para_4_each_pe                             &
!!     &         (c_size, ipe, jpe, kpe, nb_rng)
!!      subroutine set_range_4_neighbour(ipe, jpe, kpe, nb_rng)
!!      subroutine set_range_4_nodeloop(c_size, kpe, nb_rng)
!!      subroutine set_edge_para_4_each_pe(kpe, ndz, nb_rng)
!!        type(size_of_cube), intent(in) :: c_size
!!        type(neib_range_cube), intent(inout) :: nb_rng
!
      module t_neib_range_cube
!
      use m_precision
!
      implicit  none
!
      type neib_range_cube
        real(kind=kreal)  ::  xoff
        real(kind=kreal)  ::  yoff
!        real(kind=kreal)  ::  zoff
!
        integer(kind = kint)  ::  ioff
        integer(kind = kint)  ::  joff
        integer(kind = kint)  ::  koff
!
!
        integer(kind = kint)  ::  inp_st
        integer(kind = kint)  ::  inp_end
!
        integer(kind = kint)  ::  jnp_st
        integer(kind = kint)  ::  jnp_end
!
        integer(kind = kint)  ::  knp_st
        integer(kind = kint)  ::  knp_end
!
!
        integer(kind = kint)  ::  i_st
        integer(kind = kint)  ::  i_end
!
        integer(kind = kint)  ::  j_st
        integer(kind = kint)  ::  j_end
!
        integer(kind = kint)  ::  k_st
        integer(kind = kint)  ::  k_end
!
!
        integer(kind = kint)  ::  iedge_st
        integer(kind = kint)  ::  iedge_end
!
        integer(kind = kint)  ::  jedge_st
        integer(kind = kint)  ::  jedge_end
!
        integer(kind = kint)  ::  kedge_st
        integer(kind = kint)  ::  kedge_end
      end type neib_range_cube
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_offset_of_domain(c_size, ipe, jpe, kpe, nb_rng)
!
      use t_size_of_cube
      use m_size_4_plane
      use m_cube_position
!
      type(size_of_cube), intent(in) :: c_size
      type(neib_range_cube), intent(inout) :: nb_rng
!
      integer(kind=kint)  ::  ipe, jpe, kpe
!
! ***** set coordinate off set (starting corner for pe node)
!
      nb_rng%xoff = c_size%xmin                                         &
     &           + (ipe-1) * xsize * c_size%nxi /(nx_all)
      nb_rng%yoff = c_size%ymin                                         &
     &           + (jpe-1) * ysize * c_size%nyi /(ny_all)
!
!      zoff = c_size%zmin + (kpe-1) * zsize * c_size%nzi /(nz_all)
!     if (kpe/=1) zoff =  zoff - zsize/(nz_all-1) * c_size%ndepth
!
!
       end subroutine set_offset_of_domain
!
! ----------------------------------------------------------------------
!
      subroutine init_node_para_4_each_pe                               &
     &         (c_size, ipe, jpe, kpe, nb_rng)
!
      use t_size_of_cube
!
      integer(kind=kint), intent(in)  ::  ipe, jpe, kpe
      type(size_of_cube), intent(in) :: c_size
      type(neib_range_cube), intent(inout) :: nb_rng
!
!
                     nb_rng%ioff = (ipe-1)*c_size%nxi - c_size%ndepth
                     nb_rng%joff = (jpe-1)*c_size%nyi - c_size%ndepth
                     nb_rng%koff = (kpe-1)*c_size%nzi
      if(kpe .ne. 1) nb_rng%koff = nb_rng%koff - c_size%ndepth
!
      end subroutine init_node_para_4_each_pe
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_range_4_neighbour(ipe, jpe, kpe, nb_rng)
!
      use m_size_4_plane
!
      integer(kind = kint), intent(in) :: ipe, jpe, kpe
      type(neib_range_cube), intent(inout) :: nb_rng
!
!                                       .. search neighbor pe
                       nb_rng%inp_st  = -1
      if(ipe .eq.   1) nb_rng%inp_st  =  0
                       nb_rng%inp_end =  1
      if(ipe .eq. ndx) nb_rng%inp_end =  0

                       nb_rng%jnp_st  = -1
      if(jpe .eq.   1) nb_rng%jnp_st  =  0
                       nb_rng%jnp_end =  1
      if(jpe .eq. ndy) nb_rng%jnp_end =  0

                       nb_rng%knp_st  = -1
      if(kpe .eq.   1) nb_rng%knp_st  =  0
                       nb_rng%knp_end =  1
      if(kpe .eq. ndz) nb_rng%knp_end =  0
!
      end subroutine set_range_4_neighbour
!
! ----------------------------------------------------------------------
!
      subroutine set_range_4_nodeloop(c_size, kpe, nb_rng)
!
      use m_size_of_cube
!
      implicit none
!
      integer(kind = kint), intent(in) :: kpe
      type(size_of_cube), intent(in) :: c_size
      type(neib_range_cube), intent(inout) :: nb_rng
!
!
                     nb_rng%i_st  =     1         + c_size%ndepth
                     nb_rng%i_end =  nb_rng%i_st + c_size%nxi - 1

                     nb_rng%j_st  =     1         + c_size%ndepth
                     nb_rng%j_end =  nb_rng%j_st + c_size%nyi - 1

                     nb_rng%k_st  =     1         + c_size%ndepth
      if(kpe .eq. 1) nb_rng%k_st  =     1
                     nb_rng%k_end =  nb_rng%k_st + c_size%nzi - 1
!
      end subroutine set_range_4_nodeloop
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_edge_para_4_each_pe(kpe, ndz, nb_rng)
!
      integer(kind = kint), intent(in) :: kpe, ndz
      type(neib_range_cube), intent(inout) :: nb_rng
!
!
                      nb_rng%iedge_st  = nb_rng%i_st
                      nb_rng%iedge_end = nb_rng%i_end

                      nb_rng%jedge_st  = nb_rng%j_st
                      nb_rng%jedge_end = nb_rng%j_end

                      nb_rng%kedge_st  = nb_rng%k_st
                      nb_rng%kedge_end = nb_rng%k_end
      if (kpe == ndz) nb_rng%kedge_end = nb_rng%k_end - 1

      end subroutine set_edge_para_4_each_pe
!
! ----------------------------------------------------------------------
!
      end module t_neib_range_cube
