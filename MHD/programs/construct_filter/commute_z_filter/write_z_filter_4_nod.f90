!>@file   write_z_filter_4_nod.f90
!!        module write_z_filter_4_nod
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!
!>@brief FEM shape functgions for vertical filter
!!
!!@verbatim
!!      subroutine write_filter_4_nod(node, ele, edge_z_filter,         &
!!     &         zfil_param, z_commute, dz_plane, neib_z2)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(edge_data), intent(in) :: edge_z_filter
!!        type(vert_commute_filter_param), intent(in) :: zfil_param
!!        type(vart_filter_moments), intent(in) :: z_commute
!!        type(edge_z_width), intent(in) :: dz_plane
!!        type(neighbour_data_z), intent(in) :: neib_z2
!!@endverbatim
!
      module write_z_filter_4_nod
!
      use m_precision
      use t_geometry_data
      use t_edge_data
!
      implicit none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine write_filter_4_nod(node, ele, edge_z_filter,           &
     &         zfil_param, z_commute, dz_plane, neib_z2)
!
      use t_vert_commute_filter_param
      use t_commute_filter_z
      use t_neighbour_data_z
      use t_vert_edge_width
      use m_int_commtative_filter
      use m_z_filter_values
      use const_geometry_z_commute
      use set_parallel_file_name
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(edge_data), intent(in) :: edge_z_filter
      type(vert_commute_filter_param), intent(in) :: zfil_param
      type(vart_filter_moments), intent(in) :: z_commute
      type(edge_z_width), intent(in) :: dz_plane
      type(neighbour_data_z), intent(in) :: neib_z2
!
      integer(kind = kint), parameter :: id_filter_z = 15
      integer(kind = kint) :: i, inod, iele, j, k, kf
      character(len = kchara) :: file_name
!
!
      file_name = add_dat_extension(zfil_param%filter_z_file_prefix)
      write(*,*) 'vertial filter file name:  ', file_name
      open(id_filter_z,file=file_name)
      call write_vert_plane_filter_param(id_filter_z, zfil_param)
        write(id_filter_z,'(a)') '! width of nodes for filtering'
        write(id_filter_z,'(1p3e25.15e3)') zfil_param%ncomp_mat,        &
     &               zfil_param%ncomp_mat, zfil_param%ncomp_mat
!
        write(id_filter_z,'(a)')                                        &
     &       '! filter_type for horizontal direction'
        write(id_filter_z,'(a)')                                        &
     &       trim(set_z_filter_type_name(zfil_param%iflag_filter_h))
!
        write(id_filter_z,'(a)')                                        &
     &       '! filter_type for vertical direction'
        write(id_filter_z,'(a)')                                        &
     &       trim(set_z_filter_type_name(zfil_param%iflag_filter_z))
!
        write(id_filter_z,'(a)')                                        &
     &        '! Filter width( vertical, horizontal)'
        write(id_filter_z,'(1p2e25.15e3)') zfil_param%f_width_z,        &
     &                                   zfil_param%f_width_h
!
!
      do i = 1, z_commute%ncomp_norm
        do j = 0, 2
          if(z_commute%kcomp_norm_z(i) .eq. j)                          &
     &                             xmom_ht_z(j) = z_commute%f_mom_z(i)
        end do
      end do
!
        write(id_filter_z,*) '! origianl moments for three directions'
        write(id_filter_z,'(1p3E25.15e3)') (xmom_ht_x(kf), kf=0,2)
        write(id_filter_z,'(1p3E25.15e3)') (xmom_ht_y(kf), kf=0,2)
        write(id_filter_z,'(1p3E25.15e3)') (xmom_ht_z(kf), kf=0,2)
!
!        write(id_filter_z,*) '! moments for x-direction'
!        write(id_filter_z,'(1p3E25.15e3)') (xmom_ht_x(kf), kf=0,2)
        write(id_filter_z,*) '! coefficients for x-direction of moments'
        do j=1, zfil_param%ncomp_mat
          write(id_filter_z,'(i5,1p50E25.15e3)') j,                     &
     &            (xmom_h_x(j,kf), kf=0,2)
        end do
!
!        write(id_filter_z,*) '! moments for y-direction'
!        write(id_filter_z,'(1p3E25.15e3)') (xmom_ht_y(kf), kf=0,2)
        write(id_filter_z,*) '! coefficients for y-direction of moments'
        do j=1, zfil_param%ncomp_mat
          write(id_filter_z,'(i5,1p50E25.15e3)') j,                     &
     &            (xmom_h_y(j,kf), kf=0,2)
        end do
!
        write(id_filter_z,*) '! node_id, z, dz/dzeta, diff of delta_z'
!
        do inod = 1, node%internal_node
          write(id_filter_z,'(i16,1p4E25.15e3)')                        &
     &      node%inod_global(inod), node%xx(inod,3),                    &
     &      dz_plane%delta_z_n(inod), dz_plane%delta_dz_n(inod),        &
     &      dz_plane%d2_dz_n(inod)
        end do
!
        write(id_filter_z,*)                                            &
     &       '! element_id, connectivity, dz/dzeta, diff of delta_z'
!
        do iele = 1, ele%numele
          write(id_filter_z,'(3i16,1p3E25.15e3)')                       &
     &          edge_z_filter%iedge_global(iele),                       &
     &          edge_z_filter%ie_edge(iele,1:2),                        &
     &          dz_plane%delta_z_e(iele), dz_plane%delta_dz_e(iele),    &
     &          dz_plane%d2_dz_e(iele)
        end do
!
!
!
        write(id_filter_z,*) '! node_id, neighboring_nodes'
!
        do inod = 1, node%internal_node
          write(id_filter_z,'(20i16)')                                  &
     &      node%inod_global(inod), neib_z2%nneib_nod(inod,1:2),        &
     &     (node%inod_global(inod+i-neib_z2%nneib_nod(inod,1)-1),       &
     &        i=1,zfil_param%ncomp_mat)
        end do
!
!
        write(id_filter_z,*) '! node_id, coefs of filter'
!
        do inod = 1, node%internal_node
          write(id_filter_z,'(i16,1p20E25.15e3)')                       &
     &                         node%inod_global(inod),                  &
     &                         c_filter(1:zfil_param%ncomp_mat,inod)
        end do
!
!
         write(id_filter_z,*) '! node_id, 1d_momentum on node (modified)'
!
        do inod = 1, node%internal_node
          write(id_filter_z,'(i16,1p6E25.15e3)')                        &
     &          node%inod_global(inod), xmom_int_t(inod,0:2)
        end do
!
         write(id_filter_z,*)                                           &
     &         '! node_id, diff. of 1d_momentum on node (modified)'
!
        do inod = 1, node%internal_node
          write(id_filter_z,'(i16,1p6E25.15e3)')                        &
     &          node%inod_global(inod), xmom_dt(inod,0:2)
        end do
!
!
      do k = 0, 2
         write(id_filter_z,*) '! node_id, coefs_4_filtering (modified)'
!
!
        do inod = 1, node%internal_node
          write(id_filter_z,'(2i16,1p20E25.15e3)') k,                   &
     &                        node%inod_global(inod),                   &
     &                        xmom_int(inod,1:zfil_param%ncomp_mat,k)
        end do
      end do
!
         write(id_filter_z,*) '! node_id, 1d_momentum on node (original)'
!
        do inod = 1, node%internal_node
          write(id_filter_z,'(i16,1p6E25.15e3)')                        &
     &          node%inod_global(inod), xmom_int_to(inod,0:2)
        end do
!
         write(id_filter_z,*)                                           &
     &         '! node_id, diff. of 1d_momentum on node (original)'
!

        do inod = 1, node%internal_node
          write(id_filter_z,'(i16,1p6E25.15e3)')                        &
     &          node%inod_global(inod), xmom_dot(inod,0:2)
        end do
!
      do k = 0, 2
         write(id_filter_z,*) '! node_id, coefs_4_filtering (original)'
!
!
        do inod = 1, node%internal_node
          write(id_filter_z,'(2i16,1p20E25.15e3)') k,                   &
     &                      node%inod_global(inod),                     &
     &                      xmom_int_org(inod,1:zfil_param%ncomp_mat,k)
        end do
      end do
!
      close(id_filter_z)
!
      end subroutine write_filter_4_nod
!
!   --------------------------------------------------------------------
!
      end module write_z_filter_4_nod
