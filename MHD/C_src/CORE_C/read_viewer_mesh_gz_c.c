
/* read_viewer_mesh_gz_c.c */


#include "read_viewer_mesh_gz_c.h"

static void read_listed_item_gz_viewer(int num, int *item_sf){
	int i, j, iread, nread;
	char buf[LENGTHBUF];            /* character buffer for reading line */
	int num_word[1], lbuf[1];
	
	lbuf[0] = LENGTHBUF;
	
	j = 0;
	while (j < num) {
		iread = 0;
        num_word[0] = skip_comment_gz_c(lbuf, buf);
/*		get_one_line_from_gz(lbuf, num_word, nchara, buf);*/
		for (i = 0; i < num_word[0]; i++) {
			sscanf(&buf[iread], "%d%n", &item_sf[j],&nread);
			j = j + 1;
			iread = iread + nread;
		}
/*		printf("iread %d %d %d\n",iread, num_word[0], j);*/
	}
/*	printf("iread %d end\n",iread);*/
	return;
};

static void skip_listed_item_gz_viewer(int num){
    int i, j, iread, nread, itmp;
    char buf[LENGTHBUF];            /* character buffer for reading line */
    int num_word[1], lbuf[1];
    
    lbuf[0] = LENGTHBUF;
    
    j = 0;
    while (j < num) {
        iread = 0;
        num_word[0] = skip_comment_gz_c(lbuf, buf);
/*        get_one_line_from_gz(lbuf, num_word, nchara, buf);*/
        for (i = 0; i < num_word[0]; i++) {
            sscanf(&buf[iread], "%d%n", &itmp,&nread);
            j = j + 1;
            iread = iread + nread;
        }
        /*		printf("iread %d %d %d\n",iread, num_word[0], j);*/
    }
    /*	printf("iread %d end\n",iread);*/
    return;
};

static int read_listed_stack_gz_viewer(int npe, int istart, int *item_wk, int *istack_sf){
    int ntot_item;
    int i;
    
    read_listed_item_gz_viewer(npe, item_wk);
    skip_listed_item_gz_viewer(npe);
    
    istack_sf[0] = istart;
    for (i= 0; i < npe; i++) {
        istack_sf[i+1] = istack_sf[i] + item_wk[i];
    }
    ntot_item = istack_sf[npe];
    
    return ntot_item;
}

static int read_group_stack_gz_viewer(int npe, int ngrp){
	int i, j, ip, itmp, iread, nread;
	char buf[LENGTHBUF];            /* character buffer for reading line */
	int num_word[1], nchara[1], lbuf[1];
    int ntot_item;
	
	lbuf[0] = LENGTHBUF;

    skip_listed_item_gz_viewer(npe);
    skip_listed_item_gz_viewer(npe);
    
    ntot_item = 0;
    for (ip= 0; ip < npe; ip++) {
        j = 0;
        while (j < ngrp) {
            iread = 0;
            get_one_line_from_gz(lbuf, num_word, nchara, buf);
            for (i = 0; i < num_word[0]; i++) {
                sscanf(&buf[iread], "%d%n", &itmp,&nread);
                j = j + 1;
                iread = iread + nread;
            }
        }
        ntot_item = ntot_item + itmp;
    };
	return ntot_item;
};

static void read_group_item_gz_viewer(int npe, int ngrp, int *stack_sf,
									char **name, int *item){
	int i, j, jst, jnum;
	char buf[LENGTHBUF];            /* character buffer for reading line */
	int num_word[1], nchara[1], lbuf[1];
	
	lbuf[0] = LENGTHBUF;
	
	for (j = 0; j < ngrp; j++) {
        get_one_line_from_gz(lbuf, num_word, nchara, buf);
		sscanf(buf, "%s", name[j]);
        
        read_listed_item_gz_viewer(npe, &stack_sf[j*npe+1]);
        skip_listed_item_gz_viewer(npe);
        for (i = 0; i < npe; i++){
            jst = stack_sf[npe*j+i];
            jnum = stack_sf[j*npe+i+1];
            printf("%s %d %d \n",name[j], jst, jnum);
            if(jnum > 0){
                read_listed_item_gz_viewer(jnum, &item[jst]);
            } else {
                get_one_line_from_gz(lbuf, num_word, nchara, buf);
            }
            
            stack_sf[j*npe+i+1] = stack_sf[j*npe+i] + jnum;
        }
	};
	return;
};


int read_viewer_mesh_gz_c(const char *file_name, struct viewer_mesh *mesh_s){
	char buf[LENGTHBUF];    /* array for reading line */
	int itmp, ist, num;
	int i, ierr;
	int num_word[1], lbuf[1];
	
	lbuf[0] = LENGTHBUF;
	
	printf("kemoviewer mesh file name: %s \n",file_name);
	
	ierr = open_rd_gzfile_w_flag(file_name);
	if (ierr == 1){
		return ierr;
	}
	
	/* Skip comment lines*/
	num_word[0] = skip_comment_gz_c(lbuf, buf);
	
	/*get_one_line_from_gz(lbuf, num_word, nchara, buf);*/
	sscanf(buf, "%d", &mesh_s->num_pe_sf);
	
	alloc_nummesh_viewer_s(mesh_s);
	alloc_domain_stack_viewer_s(mesh_s);
	
    mesh_s->nodpetot_viewer = read_listed_stack_gz_viewer(mesh_s->num_pe_sf, IZERO, 
                                                          mesh_s->nnod_sf, mesh_s->inod_sf_stack);
	alloc_node_viewer_s(mesh_s);
	
	for (i= 0; i < mesh_s->nodpetot_viewer; i++) {
		num_word[0] = skip_comment_gz_c(lbuf, buf);
		sscanf(buf, "%d %lf %lf %lf", &itmp, 
				&mesh_s->xx_view[i][0], 
				&mesh_s->xx_view[i][1], 
				&mesh_s->xx_view[i][2]);
	};
    
    
	printf("mesh_s->xx_view %e %e %e\n", mesh_s->xx_view[mesh_s->nodpetot_viewer-1][0],
			mesh_s->xx_view[mesh_s->nodpetot_viewer-1][1],
			mesh_s->xx_view[mesh_s->nodpetot_viewer-1][2]);
	
    mesh_s->surfpetot_viewer = read_listed_stack_gz_viewer(mesh_s->num_pe_sf, IZERO, 
                                                           mesh_s->nnod_sf, mesh_s->isurf_sf_stack);
    alloc_sf_type_viewer_s(mesh_s);
	
    for (i= 0; i < mesh_s->num_pe_sf; i++) {
        ist = mesh_s->isurf_sf_stack[i];
        num = mesh_s->isurf_sf_stack[i+1] - ist;
        read_listed_item_gz_viewer(num, &mesh_s->surftyp_viewer[ist]);
    };
	
	alloc_surface_params_s(mesh_s);
	alloc_surf_connect_viewer_s(mesh_s);
	
    read_listed_item_gz_viewer(mesh_s->num_pe_sf, mesh_s->nnod_sf);
	if( mesh_s->nnod_4_surf == 9 ){
		for (i = 0; i < mesh_s->surfpetot_viewer; i++) {
			num_word[0] = skip_comment_gz_c(lbuf, buf);
			sscanf(buf, "%d %d %d %d %d %d %d %d %d %d", &itmp, 
				&mesh_s->ie_sf_viewer[i][0], 
				&mesh_s->ie_sf_viewer[i][1], 
				&mesh_s->ie_sf_viewer[i][2], 
				&mesh_s->ie_sf_viewer[i][3], 
				&mesh_s->ie_sf_viewer[i][4], 
				&mesh_s->ie_sf_viewer[i][5], 
				&mesh_s->ie_sf_viewer[i][6], 
				&mesh_s->ie_sf_viewer[i][7], 
				&mesh_s->ie_sf_viewer[i][8]);
		}
	}
	else if( mesh_s->nnod_4_surf == 8 ){
		for (i = 0; i < mesh_s->surfpetot_viewer; i++) {
			num_word[0] = skip_comment_gz_c(lbuf, buf);
			sscanf(buf, "%d %d %d %d %d %d %d %d %d", &itmp, 
				&mesh_s->ie_sf_viewer[i][0], 
				&mesh_s->ie_sf_viewer[i][1], 
				&mesh_s->ie_sf_viewer[i][2], 
				&mesh_s->ie_sf_viewer[i][3], 
				&mesh_s->ie_sf_viewer[i][4], 
				&mesh_s->ie_sf_viewer[i][5], 
				&mesh_s->ie_sf_viewer[i][6], 
					&mesh_s->ie_sf_viewer[i][7]);
		}
	}
	else{
		for (i = 0; i < mesh_s->surfpetot_viewer; i++) {
			num_word[0] = skip_comment_gz_c(lbuf, buf);
			sscanf(buf, "%d %d %d %d %d", &itmp, 
				&mesh_s->ie_sf_viewer[i][0], 
				&mesh_s->ie_sf_viewer[i][1], 
				&mesh_s->ie_sf_viewer[i][2], 
				&mesh_s->ie_sf_viewer[i][3]);
		}
	}
	
	printf("mesh_s->ie_sf_viewer %d %d %d %d\n",
			mesh_s->ie_sf_viewer[mesh_s->surfpetot_viewer-1][0],
			mesh_s->ie_sf_viewer[mesh_s->surfpetot_viewer-1][1],
			mesh_s->ie_sf_viewer[mesh_s->surfpetot_viewer-1][2],
			mesh_s->ie_sf_viewer[mesh_s->surfpetot_viewer-1][3]);
	
    
    
    mesh_s->edgepetot_viewer = read_listed_stack_gz_viewer(mesh_s->num_pe_sf, IZERO, 
                                                           mesh_s->nnod_sf, mesh_s->iedge_sf_stack);
	alloc_edge_4_sf_viewer_s(mesh_s);
	
	if( mesh_s->nnod_4_edge == 3 ){
		for (i = 0; i < mesh_s->edgepetot_viewer; i++) {
			num_word[0] = skip_comment_gz_c(lbuf, buf);
			sscanf(buf, "%d %d %d %d", &itmp, 
				&mesh_s->ie_edge_viewer[i][0], 
				&mesh_s->ie_edge_viewer[i][1], 
				&mesh_s->ie_edge_viewer[i][2]);
		}
	}
	else{
		for (i = 0; i < mesh_s->edgepetot_viewer; i++) {
			num_word[0] = skip_comment_gz_c(lbuf, buf);
			sscanf(buf, "%d %d %d", &itmp, 
				&mesh_s->ie_edge_viewer[i][0], 
				&mesh_s->ie_edge_viewer[i][1]);
		}
	}
	
	printf("mesh_s->ie_edge_viewer %d %d\n", 
			mesh_s->ie_edge_viewer[mesh_s->edgepetot_viewer-1][0],
			mesh_s->ie_edge_viewer[mesh_s->edgepetot_viewer-1][1]);
	
    read_listed_item_gz_viewer(mesh_s->num_pe_sf, mesh_s->nnod_sf);
    read_listed_item_gz_viewer(mesh_s->num_pe_sf, mesh_s->nnod_sf);
	
	for (i = 0; i < mesh_s->surfpetot_viewer; i++) {
		num_word[0] = skip_comment_gz_c(lbuf, buf);
		sscanf(buf, "%d %d %d %d %d", &itmp, 
				&mesh_s->iedge_sf_viewer[i][0], 
				&mesh_s->iedge_sf_viewer[i][1], 
				&mesh_s->iedge_sf_viewer[i][2], 
				&mesh_s->iedge_sf_viewer[i][3]);
	}
	
    printf("mesh_s->iedge_sf_viewer %d %d %d %d\n",
           mesh_s->iedge_sf_viewer[mesh_s->surfpetot_viewer-1][0],
           mesh_s->iedge_sf_viewer[mesh_s->surfpetot_viewer-1][1],
           mesh_s->iedge_sf_viewer[mesh_s->surfpetot_viewer-1][2],
           mesh_s->iedge_sf_viewer[mesh_s->surfpetot_viewer-1][3]);

    /*  node ID for domain boundary */
	
	num_word[0] = skip_comment_gz_c(lbuf, buf);
	sscanf(buf, "%d", &itmp);
/*	printf("mesh_s->nnod_domain_sf %d\n", mesh_s->nnod_domain_sf);*/
	
	mesh_s->nnod_domain_sf = read_group_stack_gz_viewer(mesh_s->num_pe_sf, 1);
	
	alloc_domain_nod_item_viewer_s(mesh_s);
    read_group_item_gz_viewer(mesh_s->num_pe_sf, IONE, mesh_s->nod_stack_domain_sf,
                              mesh_s->domain_name_sf, mesh_s->nod_item_domain_sf);
	
	/*  surface ID for domain boundary */
	
	num_word[0] = skip_comment_gz_c(lbuf, buf);
	sscanf(buf, "%d", &mesh_s->nsurf_domain_sf);
	mesh_s->nsurf_domain_sf = read_group_stack_gz_viewer(mesh_s->num_pe_sf, 1);
	
	alloc_domain_surf_item_viewer_s(mesh_s);
    read_group_item_gz_viewer(mesh_s->num_pe_sf, IONE, mesh_s->isurf_stack_domain_sf,
                              mesh_s->domain_name_sf, mesh_s->isurf_domain_sf);
	
	/*  edge ID for domain boundary */
	
	num_word[0] = skip_comment_gz_c(lbuf, buf);
	sscanf(buf, "%d", &mesh_s->nedge_domain_sf);
	mesh_s->nedge_domain_sf = read_group_stack_gz_viewer(mesh_s->num_pe_sf, 1);
	
	alloc_domain_edge_item_viewer_s(mesh_s);
    read_group_item_gz_viewer(mesh_s->num_pe_sf, IONE, mesh_s->edge_stack_domain_sf,
                              mesh_s->domain_name_sf, mesh_s->edge_item_domain_sf);
	
	/* node group */
	
	num_word[0] = skip_comment_gz_c(lbuf, buf);
	sscanf(buf, "%d", &mesh_s->ngrp_nod_sf);
	alloc_nod_grp_stack_viewer_s(mesh_s);
	
	mesh_s->nnod_nod_sf = read_group_stack_gz_viewer(mesh_s->num_pe_sf, mesh_s->ngrp_nod_sf);
	
	alloc_nod_grp_item_viewer_s(mesh_s);
	read_group_item_gz_viewer(mesh_s->num_pe_sf, mesh_s->ngrp_nod_sf, mesh_s->nod_stack_sf,
			mesh_s->nod_gp_name_sf, mesh_s->nod_item_sf);
	
	/* element group */
	
	num_word[0] = skip_comment_gz_c(lbuf, buf);
	sscanf(buf, "%d", &mesh_s->ngrp_ele_sf);
	alloc_ele_grp_stack_viewer_s(mesh_s);
	
	
	mesh_s->nele_ele_sf = read_group_stack_gz_viewer(mesh_s->num_pe_sf, mesh_s->ngrp_ele_sf);
	alloc_ele_grp_item_viewer_s(mesh_s);
	
	read_group_item_gz_viewer(mesh_s->num_pe_sf, mesh_s->ngrp_ele_sf, mesh_s->ele_stack_sf,
			mesh_s->ele_gp_name_sf, mesh_s->ele_item_sf);
	
	
	num_word[0] = skip_comment_gz_c(lbuf, buf);
	sscanf(buf, "%d", &itmp);
	
	mesh_s->nnod_ele_sf = read_group_stack_gz_viewer(mesh_s->num_pe_sf, mesh_s->ngrp_ele_sf);
	alloc_ele_grp_nod_item_viewer_s(mesh_s);
	
	read_group_item_gz_viewer(mesh_s->num_pe_sf, mesh_s->ngrp_ele_sf, mesh_s->ele_nod_stack_sf,
			mesh_s->ele_gp_name_sf, mesh_s->ele_nod_item_sf);
	
	
	num_word[0] = skip_comment_gz_c(lbuf, buf);
	sscanf(buf, "%d", &itmp);
	
	mesh_s->nedge_ele_sf = read_group_stack_gz_viewer(mesh_s->num_pe_sf, mesh_s->ngrp_ele_sf);
	alloc_ele_grp_edge_item_viewer_s(mesh_s);
	
	read_group_item_gz_viewer(mesh_s->num_pe_sf, mesh_s->ngrp_ele_sf, mesh_s->ele_edge_stack_sf,
			mesh_s->ele_gp_name_sf, mesh_s->ele_edge_item_sf);

	/* surface group */
	
	num_word[0] = skip_comment_gz_c(lbuf, buf);
	sscanf(buf, "%d", &mesh_s->ngrp_surf_sf);
	alloc_surf_grp_stack_viewer_s(mesh_s);
	
	mesh_s->nsurf_surf_sf = read_group_stack_gz_viewer(mesh_s->num_pe_sf, mesh_s->ngrp_surf_sf);
	alloc_surf_grp_item_viewer_s(mesh_s);
	
	read_group_item_gz_viewer(mesh_s->num_pe_sf, mesh_s->ngrp_surf_sf, mesh_s->surf_stack_sf,
			mesh_s->surf_gp_name_sf, mesh_s->surf_item_sf);
	
	
	num_word[0] = skip_comment_gz_c(lbuf, buf);
	sscanf(buf, "%d", &itmp);
	
	mesh_s->nnod_surf_sf = read_group_stack_gz_viewer(mesh_s->num_pe_sf, mesh_s->ngrp_surf_sf);
	alloc_surf_grp_nod_item_viewer_s(mesh_s);
	
	read_group_item_gz_viewer(mesh_s->num_pe_sf, mesh_s->ngrp_surf_sf, mesh_s->surf_nod_stack_sf,
			mesh_s->surf_gp_name_sf, mesh_s->surf_nod_item_sf);
	
	
	num_word[0] = skip_comment_gz_c(lbuf, buf);
	sscanf(buf, "%d", &mesh_s->nedge_surf_sf);
	
	mesh_s->nedge_surf_sf = read_group_stack_gz_viewer(mesh_s->num_pe_sf, mesh_s->ngrp_surf_sf);
	alloc_surf_grp_edge_item_viewer_s(mesh_s);
	
	read_group_item_gz_viewer(mesh_s->num_pe_sf, mesh_s->ngrp_surf_sf, mesh_s->surf_edge_stack_sf,
			mesh_s->surf_gp_name_sf, mesh_s->surf_edge_item_sf);
		
	close_gzfile();                                /* close file */

	return ierr;
};
